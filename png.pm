
package png;

use warnings;
use strict;
use 5.20.0;
use bytes;
use String::CRC32;
use Compress::Zlib;
# use Compress::Zlib::Perl; # doesn't work; too bad
use List::Util;
use IO::Handle;

sub read {

    my $arg = shift or die;
    my $png;
    if( ref $arg ) {
        # try it as a filehandle
        read $arg, $png, -s $arg;
    } elsif( unpack('N', $arg) != 0x89504E47 and -f $arg ) {
        # filename
        open my $fh, '<', $arg or die "$arg: $!";
        read $fh, $png, -s $fh;
    } else {
        # try it as png data
        $png = $arg;
    }
    
    my $width;
    my $height;
    my $bitdepth;
    my $colortype;
    
    my $filter;
    my $pixelsread = 0;
    my $compresseddatabuffer;
    
    my @row;
    my @previous_row;

    my $img;
    my $add_row_to_img;
    
    # PNG signature - 8bytes, big endian
    (my $x1, my $x2) = unpack 'NN', $png;
    $x1 == 0x89504E47 or die;
    $x2 == 0x0D0A1A0A or die;
    substr $png, 0, 8, '';

    my $three_pixels = sub {
        # the pixel to our left, the pixel above us to the left, and the pixel directly above us
        my $x = $pixelsread % $width;
        my $p1 = @row ? $row[$x-1] : [ 0, 0, 0, 0 ];
        my $p2 = @previous_row && @row ? $previous_row[$x-1] : [ 0, 0, 0, 0 ];
        my $p3 = @previous_row ? $previous_row[$x] : [ 0, 0, 0, 0 ];
        return ( $p1, $p2, $p3 );
    };
    
    while( $png ) {
        (my $type, my $data) = _takeApartChunk( $png );
        if( $type eq 'IHDR' ) {
    
            # warn "IHDR";
            # Width:              4 bytes
            # Height:             4 bytes
            # Bit depth:          1 byte
            # Color type:         1 byte
            # Compression method: 1 byte
            # Filter method:      1 byte
            # Interlace method:   1 byte
            ($width, $height, $bitdepth, $colortype, my $compressionmethod, my $filtermethod, my $interlacemethod) = unpack 'NNCCCCC', $data;
            $compressionmethod and die "sorry, can't handle non-standard compression method";
            $filtermethod and die "sorry, can't handle filter";
            $interlacemethod and die "sorry, can't handle interlace";
            $colortype == 2 || $colortype == 6 or die "can't handle colortype $colortype; only 2=RGB and 6=RGBA";
            # warn "width = $width, height = $height, bitdepth = $bitdepth, colortype = $colortype";
            $bitdepth == 8 or die "can't handle bitdepths other than 8";
            # eg:  width = 1440, height = 900, bitdepth = 8, colortype = 2 at pngRead.pl line 39.
            # print "P3\n$width $height\n255\n";  # absolutely works amazingly enough

            my $pixel_bytes = ( $colortype == 2 ? 3 : 4 );
            my $row_bytes = $width * $pixel_bytes + 1;      # each row has a one byte 'filter' leading
            $img = chr(0) x ( $height * $row_bytes );       # initialize output so this big block all gets allocated in RAM at once
            my $last_row = 0;
            $add_row_to_img = sub {
                my $new_row_data = chr(0) . pack 'C*', map $_->@*, @row;    # if alpha, 0, R, G, B, A, R, G, B, A... otherwise, 0, R, G, B, R, G, B...  where 0 is the pre-processing filter; we always use 0 = no filter
                length($new_row_data) == $row_bytes or die;
                # store as one long char string
                substr $img, $row_bytes * $last_row, $row_bytes, $new_row_data;
                $last_row++;
            };
    
        } elsif( $type eq 'IDAT' ) {
    
            # image data
            # warn "IDAT chunk; pre decompress, data is length @{[ length $data ]}";
            $compresseddatabuffer .= $data;
    
        } elsif( $type eq 'tEXt' ) {
    
            warn "tEXt: $data\n";
    
        } elsif( $type eq 'IEND' ) {
    
            # warn "IEND";
            my $data = Compress::Zlib::uncompress( $compresseddatabuffer );
    
            while( length $data ) {

                my $r;  my $g;  my $bl;

                if( ! ( $pixelsread % $width ) ) {
                    # remove the stupid filter method byte
                    $filter = ord substr $data, 0, 1, ''; # "filter"; should be null
                    grep $filter eq $_, 0, 1, 2, 3, 4 or warn "eek, can't handle non-standard per-row filter ``$filter''";
                    @previous_row = @row;
                    $add_row_to_img->() if @row; 
                    @row = ();
                }
    
                ($r, $g, $bl) = unpack 'CCC', $data;   substr $data, 0, 3, '';
                my $alpha = undef;
                if( $colortype == 6 ) {
                    # alpha channel
                    ($alpha) = unpack 'C', $data;
                    substr $data, 0, 1, '';
                }
    
                my $pre_pixel = [$r, $g, $bl];
    
                if ( $filter == 1 ) {
    
                    # warn "left";
                    # value of previous pixel was subtracted out
                    my( $aa, $bb, $cc ) = $three_pixels->();
                    $r += $aa->[0]; $g += $aa->[1]; $bl += $aa->[2];
                    for ($r, $g, $bl) { $_ %= 256 }
                    if( defined $alpha ) {
                        $alpha += $aa->[3]; $alpha %= 256;
                    }
    
                } elsif ( $filter == 2 ) {
    
                    # warn "up";
                    # value of above pixel was subtracted out
                    my( $aa, $bb, $cc ) = $three_pixels->();
                    $r += $cc->[0]; $g += $cc->[1]; $bl += $cc->[2];
                    for ($r, $g, $bl) { $_ %= 256 }
                    if( defined $alpha ) {
                        $alpha += $cc->[3]; $alpha %= 256;
                    }
    
                } elsif ( $filter == 3 ) {

                    # warn "average";
                    # Average
                    my( $aa, $bb, $cc ) = $three_pixels->();
                    $r += int(($aa->[0] + $cc->[0])/2);  $r %= 256;
                    $g += int(($aa->[1] + $cc->[1])/2);  $g %= 256;
                    $bl += int(($aa->[2] + $cc->[2])/2);  $bl %= 256;
                    if( defined $alpha ) {
                        $alpha += int(($aa->[3] + $cc->[3])/2); $alpha %= 256;
                    }
    
                } elsif ( $filter == 4 ) {
                    # Paeth
                    # warn "paeth";
                    my( $aa, $cc, $bb ) = $three_pixels->();    # changed order; a = left, b = above, c = above-left
                    my $pixel = defined $alpha ? [ $r, $g, $bl, $alpha ] : [$r, $g, $bl];
                    for my $byte (0..$#$pixel) {
                        my $p = $aa->[$byte] + $bb->[$byte] - $cc->[$byte];
                        my $pa = abs($p - $aa->[$byte]);
                        my $pb = abs($p - $bb->[$byte]);
                        my $pc = abs($p - $cc->[$byte]);
                        my $value = ($pa <= $pb && $pa <= $pc) ? $aa->[$byte] : ($pb <= $pc) ? $bb->[$byte] : $cc->[$byte];
                        $pixel->[$byte] += $value; 
                        $pixel->[$byte] &= 255;
                    }
                    $r = $pixel->[0]; $g = $pixel->[1]; $bl = $pixel->[2]; $alpha = $pixel->[3] if exists $pixel->[3];
                }

                if( $colortype == 6 ) {
                    push @row, [$r, $g, $bl, $alpha];
                } else {
                    push @row, [$r, $g, $bl];
                }
                $pixelsread++;
            }
            $add_row_to_img->();   # catch that last line of pixels
            $height * $width != $pixelsread and warn "missing image data?  too much image data?  $pixelsread vs @{[ $height * $width ]} specified";
        } else {
           warn "unknown chunk type ``$type''";
        }
    }

    return bless {
        img => $img, 
        colortype => $colortype,
        width => $width,
        height => $height,
    }, 'png';
 
}

sub write {

    my $img = shift;

    # settings
    my $width = $img->{width};
    my $height = $img->{height};
    my $colortype = $img->{colortype};

    # Write PNG signature - 8bytes, big endian
    my $png = pack( 'NN', 0x89504E47,0x0D0A1A0A );
    
    # Build IHDR chunk
    # Width:              4 bytes
    # Height:             4 bytes
    # Bit depth:          1 byte
    # Color type:         1 byte
    # Compression method: 1 byte
    # Filter method:      1 byte
    # Interlace method:   1 byte
    my $IHDR = pack( 'NNCCCCC', $width, $height, 0x08, $colortype, 0x00, 0x00, 0x00 ); # width, height, bit depth = 8, colortype = 2 RGB or 6 RGBA, compressionmethod = 0, filtermethod = 0, interlacemethod = 0
    $png .= _buildChunk( 0x49484452, $IHDR );

    # Build IDAT chunk
    my $IDAT = $img->{img};
    $png .= _buildChunk( 0x49444154, compress($IDAT) );
    
    # Build IEND chunk
    $png .= _buildChunk( 0x49454E44 );
    
    return $png;
}

sub blit {
    my $target = shift or die;
    my $img = shift or die;
    my $x = shift;
    my $y = shift;
    $target->isa('png') or die;
    $img->isa('png') or die;

    my $img_pixel_bytes = ( $img->{colortype} == 2 ? 3 : 4 );
    my $img_row_bytes = $img->{width} * $img_pixel_bytes + 1;      # each row has a one byte 'filter' leading

    my $target_pixel_bytes = ( $target->{colortype} == 2 ? 3 : 4 );
    my $target_row_bytes = $target->{width} * $target_pixel_bytes + 1;

    # warn "img_row_bytes = $img_row_bytes target_row_bytes = $target_row_bytes\n";

    my $x__ = 0;   # coords in $img
    my $y__ = 0;

    for(my $y_ = $y; $y_ < $y + $img->{height}; $y_++ ) { 
        for(my $x_ = $x; $x_ < $x + $img->{width}; $x_++ ) { 
            next if $x_ < 0 or $x_ >= $target->{width};
            next if $y_ < 0 or $y_ >= $target->{height};

            my $pixel = substr $img->{img}, $img_row_bytes * $y__ + $img_pixel_bytes * $x__ + 1, $img_pixel_bytes;

            if( $img_pixel_bytes == 4 ) {
                # image we're copy has alpha
                my $alpha = unpack 'C', substr $pixel, 3, 1, '';     # but we always write three bytes and don't change the target alpha
                my $target_pixel = substr $target->{img}, $target_row_bytes * $y_ + $target_pixel_bytes * $x_ + 1, 3;
                my ($r1, $g1, $b1) = unpack 'CCC', $pixel;
                my ($r2, $g2, $b2) = unpack 'CCC', $target_pixel;
                my $r3 = int((  $r1 * $alpha + ( $r2 * ( 255 - $alpha ) ))/255); $r3 = 255 if $r3 > 255;
                # warn " int((  $r1 * $alpha + ( $r2 * ( 255 - $alpha ) ))/255) = $r3\n";
                my $g3 = int((  $g1 * $alpha + ( $g2 * ( 255 - $alpha ) ))/255); $g3 = 255 if $g3 > 255;
                my $b3 = int((  $b1 * $alpha + ( $b2 * ( 255 - $alpha ) ))/255); $b3 = 255 if $b3 > 255;
                $pixel = pack 'CCC', $r3, $g3, $b3;
            }

            substr $target->{img}, $target_row_bytes * $y_ + $target_pixel_bytes * $x_ + 1, 3, $pixel;

            $x__++;
        }
        $y__++;
        $x__ = 0;
    }
}

sub _buildChunk {
	my $type = shift;
	my $data = shift || "";
	# length
	my $length = pack( 'N', length $data ); # 0 is valid
	# chunk type and data
	my $chunk = pack( 'N', $type ) . $data;
	# CRC
	return $length . $chunk . pack( 'N', crc32($chunk) );
}
    
sub _takeApartChunk {
    (my $length) = unpack 'N', $_[0];     substr $_[0], 0, 4, '';
    my $type = substr $_[0], 0, 4, '';
    my $data = substr $_[0], 0, $length, '';
    (my $crc) = unpack 'N', $_[0];     substr $_[0], 0, 4, '';
    crc32($type . $data) eq $crc or die "CRC; type = $type; length = $length";
    return ($type, $data);
}

if( 0 and $0  eq __FILE__ ) {
    # simple read/write the energy cube which uses paeth and other compression filters (thanks, gimp, not)
    my $img5 = png::read("jpg/energy.png");
    # write for inspection
    my $png = $img5->write;
    open my $imgfh, '>', "test.png" or die $!;
    $imgfh->print($png);
    close $imgfh;
}

if( $0  eq __FILE__ ) {
    # test blitting the robot and energy cubes around on the board and saving the modified board

    my $img1 = png::read("jpg/startb_and_spinzone_b.png");
    my $img2 = png::read("jpg/bot1.png");
    my $img3 = png::read("jpg/bot2.png");
    my $img4 = png::read("jpg/bot3.png");
    my $img5 = png::read("jpg/energy.png");

    $img1->blit($img2, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img3, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img4, 69 * int rand 15, 69 * int rand 11);

    $img1->blit($img5, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img5, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img5, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img5, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img5, 69 * int rand 15, 69 * int rand 11);
    $img1->blit($img5, 69 * int rand 15, 69 * int rand 11);

    # write for inspection
    my $png = $img1->write;
    open my $imgfh, '>', "test.png" or die $!;
    $imgfh->print($png);
    close $imgfh;
}

if( 0 and $0  eq __FILE__ ) {
    require Test::Deep; Test::Deep->import;
    opendir my $dir, 'jpg' or die $!;
    while( my $fn = readdir $dir ) {
        next if $fn =~ m/^\./;
        next unless $fn =~ m/.png$/; 
        warn "$fn:\n";
        # do a round trip on the data... decompress the file, compress it again, decompress that again, and compare it to what we first decompressed.
        open my $fh, '<', "jpg/$fn" or die $!;
        my $img1 = png::read($fh);
        # use Data::Dumper; die Dumper $img1;
        my $png = png::write($img1);
        my $img2 = png::read($png);
        cmp_deeply($img1, $img2);
        # spit out a png for other readers to test
        open my $imgfh, '>', "test_$fn" or die $!;
        $imgfh->print($png);
        close $imgfh;
    }
    done_testing();
}

1;
