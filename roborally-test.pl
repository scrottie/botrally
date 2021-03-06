#!/usr/local/bin/perl5.28.3

use 5.20.0;

use strict;
use warnings;

use Data::Dumper;

no warnings 'once';
no warnings 'redefine';

use Test::More;

$main::test = 1;   # don't run the event loop

use lib '.';
require 'roborally.pl';

#

my $p1 = player->new( player_id => 'scott', password => 'weee' ) or die;
my $p2 = player->new( player_id => 'fred', password => 'weee' ) or die;

#

*board::tiles = sub {
    return [
        [qw/open open open open open open/, undef],                               # 0
        [qw/open open open open open open/, undef],                               # 1
        [qw/open convey_ne convey_e convey_es open open/, undef],                 # 2
        [qw/open convey_n open convey_s open open/, undef],                       # 3
        [qw/open convey_wn convey_w convey_sw open open/, undef],                 # 4
        [qw/open wall_s wall_w wall_e wall_n wall_w /, undef],                    # 5
        [],
    ];
};

#

my $board = $p1->board;  # both return the same

is $board->width_tiles, 6, 'board width' or die;
is $board->height_tiles, 6, 'board height' or die;

#

do {
    (my $r1, my $r2) = grep $_->isa('robot'), $p1->board->cards or die;
    $r1->moved($p1);  # associate
    $r2->moved($p2);
    $p1->robot or die;
    $p2->robot or die;
};

$p1->robot->colrow(0, 0);
$p2->robot->colrow(0, 1);

$p1->robot->orientation = 'e';
$p2->robot->orientation = 'e';

#

#

# XXX lots of fun stuff to test =)

# convey_dbl_e convey_dbl_es convey_dbl_n convey_dbl_ne convey_dbl_s convey_dbl_sw convey_dbl_w convey_dbl_wn convey_en convey_es convey_n convey_ne convey_s convey_se
# laser_wall_e laser_wall_n laser_wall_s laser_wall_w
# open
# power
# rotate_left  rotate_right
# spawn spawn_wall_n spawn_wall_s 
# wall_e wall_n wall_nw wall_s wall_sw wall_w

# simple move 1

put_card_in_register($p1, 'move1', 1);
$p1->robot->colrow(0, 0);
$p2->robot->colrow(0, 1);
$p1->robot->orientation = 'e';
is $p1->damage, 0 or die;

main::simulate(1);

is $p1->robot->col, 1, 'move1 x' or die;
is $p1->robot->row, 0, 'move1 y' or die;
is $p1->robot->orientation, 'e' or die;
is $p1->damage, 0 or die;

# s wall 

put_card_in_register($p1, 'move1', 1);
$p1->robot->colrow(1, 5);
$p1->robot->orientation = 's';
is $p1->damage, 0 or die;

main::simulate(1);

is $p1->robot->col, 1, 'move1 x in to s wall' or die;   # unmoved from start
is $p1->robot->row, 5, 'move1 y in to s wall' or die;

# e wall

put_card_in_register($p1, 'move1', 1);
$p1->robot->colrow(3, 5);
$p1->robot->orientation = 'e';
is $p1->damage, 0 or die;

main::simulate(1);

is $p1->robot->col, 3, 'move1 x in to e wall' or die;   # unmoved from start
is $p1->robot->row, 5, 'move1 y in to e wall' or die;

# w wall on tile we're trying to enter going east

put_card_in_register($p1, 'move1', 1);
$p1->robot->colrow(4, 5);
$p1->robot->orientation = 'e';
is $p1->damage, 0 or die;

main::simulate(1);

is $p1->robot->col, 4, 'move1 x e in to w wall' or die;   # unmoved from start
is $p1->robot->row, 5, 'move1 y e in to e wall' or die;
is $p1->robot->orientation, 'e' or die;

# south of conveyer, walking on to conveyer

put_card_in_register($p1, 'move1', 1);
$p1->robot->colrow(3, 5);
$p1->robot->orientation = 'n';
is $p1->damage, 0 or die;

main::simulate(1);

is $p1->robot->col, 2, 'move1 x' or die;        # walked n on to conveyer which is a convey_sw, get conveyed one west, not rotated; wound up one n and one w of where we started, facing the same direction
is $p1->robot->row, 4, 'move1 y' or die;
is $p1->robot->orientation, 'n';
is $p1->damage, 0 or die;

put_card_in_register($p1, 'left', 1);           # scenario continued

main::simulate(1);

is $p1->robot->col, 1, 'move1 x' or die;        # rotated left to face w, conveyer moved once more and rotated us n
is $p1->robot->row, 4, 'move1 y' or die;
is $p1->robot->orientation, 'n';
is $p1->damage, 0 or die;

# competing for a space to test priority

put_card_in_register($p1, 'move1', 1);          # closer to tower so should move first
$p1->robot->colrow(0, 1);
$p1->robot->orientation = 'e';

put_card_in_register($p2, 'move1', 1);          # but bad player moves in to the same spot second and pushes p1 back; both robots shoot each other once
$p2->robot->colrow(2, 1);
$p2->robot->orientation = 'w';

main::simulate(1);

is $p1->robot->col, 0, 'move1 x' or die;
is $p1->robot->row, 1, 'move1 y' or die;
is $p1->robot->orientation, 'e';
is $p1->damage, 1 or die;

is $p2->robot->col, 1, 'move1 x' or die;
is $p2->robot->row, 1, 'move1 y' or die;
is $p2->robot->orientation, 'w';
is $p2->damage, 1 or die;

clear_register($p2, 1);

# march off the board and take spam damage... oh, crud, won't take spam damage because we're doing that outside of the call to simulate() currently... have to move post-five-founds-cleanup to a sub first.

put_card_in_register($p1, 'move1', 1);
$p1->robot->colrow(5, 1);
$p1->robot->orientation = 'e';

main::simulate(1);

is $p1->robot->col, 6, 'move1 x' or die;
is $p1->robot->row, 1, 'move1 y' or die;
is $p1->robot->orientation, 'e';
ok ! $board->on_the_board($p1->robot);

#

# I call this one, "sit and spin".

*board::tiles = sub {
    return [
        #   0    1            2            3            4    5
        [qw/open open         laser_wall_n open         open open/, undef],                       # 0
        [qw/open laser_wall_w rotate_right laser_wall_e open open/, undef],                       # 1
        [qw/open open         laser_wall_s open         open open/, undef],                       # 2
        [],
    ];
};

clear_register($p1, 1);
is $p1->damage, 1, 'sit and spin' or die;
$p1->robot->colrow(2, 1);    # into the line of fire
$p2->robot->colrow(0, 0);    # out of the line of fire
$p1->robot->orientation = 'e';

main::simulate(1);

is $p1->robot->col, 2, 'sit and spin' or die;
is $p1->robot->row, 1, 'sit and spin' or die;
is $p1->robot->orientation, 's';
ok $board->on_the_board($p1->robot) or die;
is $p1->damage, 5, 'sit and spin' or die;       # ouch


#
#
#

done_testing;


#
#
#

sub put_card_in_register {
    my $player = shift;
    my $name = shift or die;
    my $reg = shift or die;
    my @reg_offsets = $player->reg_offsets;  # same for both
    (my $card) = grep $_->card_action eq $name, $player->cards or die;
    $card->xy($player->x + $reg_offsets[1]->[0], $player->y + $reg_offsets[1]->[1]);
    $player->dropped_on_us($card, $player);
}

sub clear_register {
    my $player = shift;
    my $register = shift;
    my @registers = $player->registers;
    if( my $card = $registers[$register] ) {
        $card->xy($player->x + 120, $player->y + 46);    # discard area currently doesn't have variables, but that's the discard area
    }
}

