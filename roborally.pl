#!/usr/local/bin/perl5.28.3

use Continuity;
use Continuity::Adapt::HttpDaemon;
use Continuity::Mapper;

use Coro;
use Coro::Event;

use IO::Handle;
use List::Util 'shuffle', 'min', 'max';
use Scalar::Util 'blessed';
use Storable 'nstore', 'retrieve';
use Image::Info;
use JSON;
use Data::Dumper;
use FindBin '$Bin';
use Carp;
use Try::Tiny;

use lib '.';
use png;

use strict;
use warnings;
no warnings 'once';


#          .__        ___.          .__              __          __          
#     ____ |  |   ____\_ |__ _____  |  |     _______/  |______ _/  |_  ____  
#    / ___\|  |  /  _ \| __ \\__  \ |  |    /  ___/\   __\__  \\   __\/ __ \ 
#   / /_/  >  |_(  <_> ) \_\ \/ __ \|  |__  \___ \  |  |  / __ \|  | \  ___/ 
#   \___  /|____/\____/|___  (____  /____/ /____  > |__| (____  /__|  \___  >
#  /_____/                 \/     \/            \/            \/          \/

# misc config

my $poll_time = 30; # how long to dally answering update requests when no updates are available

# error handling

use Carp; $SIG{__DIE__} = sub { confess @_; };
$SIG{PIPE} = sub { };

# server

my $server = Continuity->new( 
    # adaptor => Continuity::Adapt::HttpDaemon->new( LocalPort => 8000, ), 
    # staticp => sub { $_[0]->url->path =~ m/(jpg|gif|png|css|js)$/; },
    # staticp => sub { $_[0]->url =~ m/(jpg|gif|png|css|js)$/; },
    port => 11111,
    callback => sub { main(@_) },
    debug => 1,
    mapper   => Continuity::Mapper->new(
        callback => \&main,
        ip_session => 1,
        path_session => 1,                      # requests for /board etc are things that are satisified quickly, and requests for /updator are things that block
        # cookie_session => 'sid',
    ),
);

# my $url = "http://192.168.0.15:11704";
# my $url = `hostname` =~ m/fluffy/ ? 'http://74.76.174.189:11111' : 'http://45.79.14.228:11111/'; # XXX
# my $url = $server->adapter->daemon->url;
my $url = `hostname` =~ m/fluffy/ ? 'http://127.0.0.1:11111' : 'http://45.79.14.228:11111/'; # XXX
warn $url;

# movement deltas

my @actions; # history of all movements used to send deltas -- contains intances of the action class.

# players

my @players;

# .__                              __   
# |  | _____  ___.__. ____  __ ___/  |_ 
# |  | \__  \<   |  |/  _ \|  |  \   __\
# |  |__/ __ \\___  (  <_> )  |  /|  |  
# |____(____  / ____|\____/|____/ |__|  
#           \/\/       

# game board is the entire play area and tracks everything in the play area
# game board is also the main play area

my $board = board->new(
    board_x => 2000, board_y => 2000, id => 'board', front_img => '/jpg/startb_and_spinzone_b.png', id => 'board', x => 10, y => 30, stationary => 1,
);
$board->add( $board );
$board->move_past;

my $past_board_x = $board->cur_x + 20;

# board energy (which is diff than the energy bank)

do {
    for my $col (0..15) {
        for my $row (0..11) {
            if( $board->tiles->[ $row ]->[ $col ] =~ m/power/ ) {
                my $card = energy->new;
                $card->row = $row;
                $card->col = $col;
                $board->add($card); 
                # push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'board setup');  # move; don't have to do this if cards positions are set before the player loads the board
            }
        }
    }
};

# checkpoints

my @checkpoints;

for my $i (1..5) {
    # checkpoints get scattered on the board
    my $card = checkpoint->new( id => "checkpoint$i", checkpoint_num => $i, front_img => "/jpg/checkpoint$i.png", back_img => "/jpg/checkpoint$i.png", );
    $board->add($card); $board->cur_x += 5;
    push @checkpoints, $card;
}
$board->move_past;

# bots

# after checkpoints only so that they appear on top of checkpoints

for my $i (1..3) {
    # robots associate themselves with the player moving them, the first time they are moved
    my $bot = robot->new( front_img => "/jpg/bot$i.png", back_img => "/jpg/bot$i.png", id => [undef, 'SmashBot', 'Hulkx90', 'HammerBot']->[$i], );
    $board->add( $bot ); $board->move_past;
    push @actions, action->new( type => 2, card => $bot, player_id => 'system', );   # rotate to default starting rotation
}

# creates

my @crates;
my $last_crate_deployed = 0;
my $crate_counter = 0;
$board->move_past();
for my $i (0..20) {
    my $crate = crate->new(id => "crate$i");  # all in the same space
    $board->add( $crate );   
    $crates[$i] = $crate;
};

$board->new_line();   # just to clear board->last_card_height and reset it back to 0

# special movement cards (purchased via upgrade cards) 

my @special_move;
$board->cur_x = $past_board_x;
$board->cur_y = 80;
# XXX need multi-column layout or explicitly dividing up the area in to arbitrary panes
do {
    my $i = 1;
    for my $fn (
        List::Util::shuffle(qw/
            card-special-spamfolder.png
            card-special-energyroutine.png
            card-special-speedroutine.png
            card-special-repeatroutine.png
        /)
    ) {
        # XXX these two need their corresponding upgrade card scanned and added to the upgrade cards deck
        my $card = upgrade->new( front_img => "/jpg/$fn", back_img => "/jpg/cardback.png", id => "specialmove$i", hid => 0, );
        $board->add($card);
        $board->move_past;
        push @special_move, $card;
        $i++;
    }
};

# upgrade cards

my @upgrades;
$board->cur_x = $past_board_x;
$board->cur_y = 249;
(my $upgrade_x, my $upgrade_y) = ($board->cur_x, $board->cur_y);

do {
    my $i = 1;
    for my $fn (
        List::Util::shuffle(qw/
            upgrade-doublebarrellaser.png
            upgrade-railgun.png
            upgrade-deflectershield.png
            upgrade-defrag.png
            upgrade-firewall.png
            upgrade-memorystickc.png
            upgrade-ramminggear.png
            upgrade-rearlaser.png
            upgrade-teleporter.png
            upgrade-pressorbeam.png
            upgrade-tractorbeam.png
            upgrade-recharge.png
            upgrade-recompile.png
            upgrade-spamblocker.png
            upgrade-zoop.png
            upgrade-energyroutine.png
            upgrade-spamfolder.png
            upgrade-repeatroutine.png
            upgrade-speedroutine.png
        /)
    ) {
        # energyroutine, spamfolder, repeatroutine, speedroutine have corresponding special movement cards
        # XXX not implemented but possible to automate:
        #    upgrade_hoverunit.png
        #    upgrade-corruptionwave.png     -- easier to automatically move spam to other people's decks but not sure I'm reading this one right... this seems really powerful... that basically permanently eliminates spam for its owner
        #    upgrade_scrambler.png
        my $card = upgrade->new( front_img => "/jpg/$fn", back_img => "/jpg/cardback.png", id => "upgrade$i", hid => 1, );
        $board->add($card);
        $board->cur_x += 15;
        push @upgrades, $card;
        $i++;
    }
};

#

$board->new_line();   # just to clear board->last_card_height and reset it back to 0

# below the game board, use layout for the pads, and card pile

$board->cur_x = 0;  $board->cur_y = 828 + 30 + 4;   # restart layout here tho may not still be needed tho a lot of stuff is still fixed position

# action pads

(my $pad_width, my $pad_height) = (170, 170); 

$board->add( checkpointspad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

# $board->add( turnoverpad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

$board->add( rotateleftpad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

$board->add( rotaterightpad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

$board->add( shufflepad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

$board->add( dealpad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

$board->add( startpad->new( card_width => $pad_width, card_height => $pad_height ) );  $board->move_past;

$board->add( my $dog = card->new( front_img => '/jpg/dog.png', back_img => '/jpg/dog.png', id => 'dog' ) );  $board->move_past;
(my $dog_x, my $dog_y) = ($dog->x, $dog->y);

# players mat

$board->new_line();

(my $tray_x, my $tray_y) = ($board->cur_x, $board->cur_y);
# per-player mat added on player create.  just note where it should be put.

# movement card draw deck

(my $deck_x, my $deck_y) = ($tray_x + 604, $tray_y + 90);

# movement card discard pile

(my $discard_x, my $discard_y) = ($tray_x + 7, $tray_y + 90);


# spam deck, past the player's mat

$board->cur_x = 920;
$board->cur_y += 10;  # plus move down a little more just so that the cards aren't just right against the bottom of the pads

(my $spam_x, my $spam_y) = ($board->cur_x, $board->cur_y);
my $spam_last_x;   # for locating cards still in the draw pile
my @spam_cards;

do {

    for(1..36) {
        my $card = spam->new( front_img => '/jpg/card-spam.png', back_img => '/jpg/cardbacksilver.png', hid => 0, );
        push @spam_cards, $card;
        $board->add($card); 
    }

    sub {

        # this is initialization as well as reset
        # XXX during reset, initialize player's decks as well, for all players

        # (my $start_z) = map $_->z, min grep $_->z, @cards;
        my $start_z = $board->next_z;

        $spam_last_x = $spam_x;

        for my $card (@spam_cards) {
            $card->x = $spam_last_x += 2;
            $card->y = $spam_y;
            $card->z = $start_z++;
            # push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'stack');  # testing not doing this during early setup XXX
        }

    };

}->();

# energy bank, to the right of the spam deck, and energy pad for energy bank

(my $energy_x, my $energy_y) = ($board->cur_x = 1314, $board->cur_y = 1063);
(my $energy_max_x, my $energy_max_y) = ($energy_x + 200, undef );
my @energy;

do {
    $board->cur_x = $energy_x;
    $board->cur_y = $energy_y;
    for( 1 .. 50 ) {
        my $card = energy->new;
        $board->cur_x += 35;
        if( $board->cur_x + $card->card_width > $energy_max_x ) {
            # next row
            $board->cur_x = $energy_x;
            $board->cur_y += 35;
        } 
        $energy_max_y = $board->cur_y + $card->card_height + 1;
        $board->add($card); 
        push @energy, $card;
        # push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'shuffle');  # move  ... these aren't actually needed during early setup
    }
    $board->add( energypad->new( x => $energy_x-5, y => $energy_y, energy_last_x => $board->cur_x, energy_last_y => $board->cur_y, ) );  $board->move_past;
};


# permanent discard

$board->add( discardpad->new( x => 900, y => 1284, ) ); $board->move_past; # next to the player's mat 

# upgrade pad

$board->add( upgradepad->new( x => 0, y => 1524, ) ); $board->move_past; # below the player's mat

# console pad

my $console = console->new( x => 900, y => 1524, ); # right of the player's upgrade mat
$board->add( $console );
$board->move_past;


#                                .__                        
#  _______ __ __  ____           |  |   ____   ____ ______  
#  \_  __ \  |  \/    \   ______ |  |  /  _ \ /  _ \\____ \ 
#   |  | \/  |  /   |  \ /_____/ |  |_(  <_> |  <_> )  |_> >
#   |__|  |____/|___|  /         |____/\____/ \____/|   __/ 
#                    \/                             |__|  


# restore game position

if(-f "$0.board.store") {
    $board = retrieve("$0.board.store");
}

# backups

async {
    my $timer = Coro::Event->timer( interval => 300, );
    my $delta = 0;
    while(1) {
        $timer->next;  # elsewhere, had to cede to keep the timer from blocking
        $delta == @actions and next;
        $delta = @actions;
        nstore $board, "$0.board.store.new";  
        rename "$0.board.store.new", "$0.board.store";
    }
};

# constants

my $deltas = {
    n => [ 0, -1 ],
    s => [ 0, 1 ],
    w => [ -1, 0 ],
    e => [ 1, 0 ],
};

my $orientation_for_deltas = {
    -1 => { 0 => 'w', },              # nw, sw not handled
    0 => {  -1 => 'n', 1 => 's', },   # ->{0}->{0} is no movement which is also undef
    1 => { 0 => 'e', },               # se, ne not handled
};

my $opdir = { n => 's', s => 'n', w => 'e', e => 'w', };

# event loop

return 1 if $main::test;

$server->loop;

sub main {
    my $request = shift;
    my $action;

    # each player will get one concurrent of this loop for each path, so the first path visited needs to pass on the password to the first request for each new path to bypass showing the login screen again for that path.
    # we need multiple paths because the updator action blocks.

    my $player;

    if( ! @players ) {
        # dev crutch... auto-create the first player
        $player = player->new( player_id => 'scott', password => 'weee' );
        push @players, $player;
        goto next_request;   # skip login screen
    }

    if( $request->param('player_id') && $request->param('password') ) {
        # skip the login screen which is needed to avoid having them login multiple times, once for each path
        ($player) = grep $_->player_id eq $request->param('player_id'), @players;
        if($player and $player->password eq $request->param('password') ) {
            goto next_request;   # skip login screen
        }
    }
    $player = login($request);
    $request = $request->next;

  next_request:
    $player or die;
    $action = $request->param('action') || 'board';
    STDERR->print('-' x 30, ' ', $request->{request}->uri, ' ', '-' x 30, "\n");
    main->can($action) and main->can($action)->($request, $player);
    $request = $request->next;
    goto next_request;
}

#        .__              .__          __          
#   _____|__| _____  __ __|  | _____ _/  |_  ____  
#  /  ___/  |/     \|  |  \  | \__  \\   __\/ __ \ 
#  \___ \|  |  Y Y  \  |  /  |__/ __ \|  | \  ___/ 
# /____  >__|__|_|  /____/|____(____  /__|  \___  >
#      \/         \/                \/          \/ 

sub recursive_move {
    my $robot = shift;
    my $x_delta = shift;
    my $y_delta = shift;
    my $objects = shift;
    my $dont_push_flag = shift;

    STDERR->print("recursive_move: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " considering move $x_delta $y_delta\n");

    my $row = $robot->row;
    my $col = $robot->col;

    # current square may prevent us from leaving; if so, return failure
    my $current_tile = $board->tiles->[ $row ]->[ $col ];
    return 0 unless $current_tile;   # we can't move any direction if we aren't on the board; someone has to pick us up and put us back
    #  my $orientation = $robot->orientation or die "no orientation";  # n, s, w, or e # orientation for deltas instead so that direction persists
    my $orientation = $orientation_for_deltas->{$x_delta}->{$y_delta};
    my $has_teleporter = $robot->player && $robot->player->has_upgrade('upgrade-teleporter');
    if( $current_tile =~ m/wall_[nswe]*${orientation}$/ ) {
        # may look like "laser_wall_ne" which should match both "wall_e" and "wall_n".  "spawn_wall_ne" and plain "wall_ne" are possibilities too.
        # return 0 unless $has_teleporter;
        if( $has_teleporter) {
            # since they have a teleporter upgrade, we know $robot->player is set
            $robot->player->spend_energy(1) or return 0;
            $console->log("robot @{[ $robot->name ]} teleported through a wall $orientation before leaving the tile\n");
            # search forward/around to find and make sure there is an open spot we can drop them so they don't get stuck XXX
            # continue on
        } else {
            $console->log("@{[ $robot->name ]} ran in to a wall going $orientation.\n");
            return 0;
        }
    }

    # where the robot will stand after moving, if they can move
    my $future_col = $col + $x_delta;
    my $future_row = $row + $y_delta;
    (my @obj) = sort { $a->z > $b->z } grep { $_->row == $future_row and $_->col == $future_col }  $objects->@*;
    my $dest_tile = $board->tiles->[ $future_row ]->[ $future_col ];

    # the poissibilities for the tile we're trying to move in to are 1. something we can move in to  2. another robot or object with $ob->push_me false  3. a wall  4. off the board or a pit which is basically just #2 
    if( $dest_tile and $dest_tile =~ m/wall_[nswe]*$opdir->{$orientation}$/ ) {
        # wall on the dest tile on the opposite side as the direction we're moving
        if( $has_teleporter ) {
            $robot->player->spend_energy(1) or return 0;
            $console->log("@{[ $robot->name ]} teleported through a wall $orientation after leaving the tile\n");
            # continue on
        } else {
            $console->log("@{[ $robot->name ]} ran in to a wall.\n");
            return 0;
        }
    }

    for my $obj (@obj) {
        if( $dest_tile and $obj and $obj->isa('robot') ) {
            if( $dont_push_flag ) {
                STDERR->print("recursive_move: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " declining to push another robot\n");
                return 0;
            }
            $obj->push_me($robot);
            STDERR->print($robot->name, ' at ', $robot->col, ', ', $robot->row, " attempting to push another robot, @{[ $obj->name ]}\n");
            # try to move the robot we ran in to
            if( recursive_move($obj, $x_delta, $y_delta, $objects) ) {
                $console->log( $robot->name . ' at ' . $robot->col . ', ' . $robot->row . " pushed @{[ $obj->name ]} @{[ $orientation_for_deltas->{$x_delta}->{$y_delta} ]}.\n");
            } else {
                $console->log( $robot->name . ' at ' . $robot->col . ', ' . $robot->row . " attempted and failed to push @{[ $obj->name ]} @{[ $orientation_for_deltas->{$x_delta}->{$y_delta} ]}.\n");
                return 0;
            }
        } elsif( $obj->step_on($robot) ) {
            STDERR->print("recursive_move: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " stepping on a non-robot object not at the end of a register:  @{[ $obj->id ]}\n");
            # continue on
        } elsif( $dont_push_flag ) {
            STDERR->print("recursive_move: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " declining to push something that isn't a robot:  @{[ $obj->id ]}\n");
            return 0;
        } elsif( $obj->push_me($robot) ) {
            # same as the robot case, just with less output
            STDERR->print("recursive_move: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " pushing a non-robot object:  @{[ $obj->id ]}\n");
            return 0 if 0 == recursive_move($obj, $x_delta, $y_delta, $objects);      # try to move the non-robot object we ran in to
        }
    }

    STDERR->print("recursive_move: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " permitted to move $x_delta $y_delta\n");

    $robot->col = $future_col;
    $robot->row = $future_row;

    $console->log( $robot->name . ' moved ' . $orientation_for_deltas->{$x_delta}->{$y_delta} . ' to '. $robot->col . ', ' . $robot->row . "\n");
    push @actions, action->new( type => 1, card => $robot, player_id => 'simulate', text => $robot->x . ', ' . $robot->y);   # move the card for everyone

    if( ! $board->on_the_board($robot) ) {
        $console->log($robot->name . " is off the board\n");
        return 1;  # you can successfully move off the board even though it may not be a good idea to do so
    }

    return 1;
}

sub lasers {
    my $attacker = shift;
    my $dir = shift;
    my $x = shift;
    my $y = shift;
    my $objects = shift;
    my $skip = shift;                      # skip this many tiles (generally 0 or 1 ) before looking for targets; robots fire in to adjacent tiles but we still need to look for walls that block fire
    my $phase = shift;                     # register phase aka $step
    my $shoot_through_walls_flag = shift;  # railgun mode
    my $double_barrel_lasers = shift;      # 2x spam damage
    my $missile_callback = shift;          # optionally do something besides laser (spam) damage
    # map { STDERR->print("object @{[ $_->id ]} at @{[ $_->col ]}, @{[ $_->row ]}\n") } @$objects;
    (my $x_delta, my $y_delta) = $deltas->{$dir}->@*;
    STDERR->print("laser firing $dir with deltas $x_delta, $y_delta starting at $x, $y @{[ $shoot_through_walls_flag ? 'with railgun' : '' ]} @{[ $double_barrel_lasers ? 'with double barrel lasers' : '' ]} @{[ $missile_callback ? 'with a custom damage routine' : '' ]}\n");
    while(1) {
        (my @bots) = grep { $_->isa('robot') and $_->col == $x and $_->row == $y } @$objects;
        if( @bots and ! $skip ) {
            for my $robot (@bots) {
                # shouldn't be more than one bot on a tile but handle the case
                if( $robot->player and $robot->player->has_upgrade('upgrade-deflectershield') and ! $missile_callback ) {
                    if( $robot->player->last_shield_phase and $robot->player->last_shield_phase == $phase ) {
                        # already have their shields up
                        $console->log("@{[ $robot->name ]} still has shields up for register phase $phase\n");
                        next;
                    } else {
                         if( $robot->player->spend_energy(1) ) {
                             $console->log("@{[ $robot->name ]} spent one energy to put shields up for this register phase $phase\n");
                             $robot->player->last_shield_phase = $phase;
                             next;
                         }
                         # continue to take damage of spending energy failed
                    }
                }
                if( $missile_callback ) {
                    # optional custom effect
                    STDERR->print("callback missile hit: damage to @{[ $robot->name ]}\n");
                    $missile_callback->($robot, $dir, $objects);
                } else {
                    $console->log("@{[ $attacker->name ]}'s @{[ $double_barrel_lasers ? 'double-barrel' : '' ]} laser@{[ $double_barrel_lasers ? 's' : '' ]}  hit @{[ $robot->name ]}, dealing spam damage!\n");
                    $robot->player->take_spam_damage if $robot->player;                 # XXX generalize this to non-robots... take_damage should be a method on the robot, not the player, tho it can relay it to the player if a robot
                    if( $double_barrel_lasers ) {
                        $robot->player->take_spam_damage if $robot->player;                 # XXX generalize this
                    }
                }
            }
            last;   # hitting a bot also stops the laser
        }
        $skip-- if $skip;
        # annoyingly, repeats the above
        # stop if the laser cannot exist the curent tile
        my $tile = $board->tiles->[ $y ]->[ $x ];
        $tile or last;   # if we went off the top/bottom/side of the board
        last if $dir eq 'e' and $tile =~ m/wall_[nsw]*${dir}/ and ! $shoot_through_walls_flag;   # east bound laser (for example) hitting an east wall prevents the laser from existing this tile.  ignoring nsw walls for the moment.
        # advance to the next tile
        $x += $x_delta; $y += $y_delta;
        $tile = $board->tiles->[ $y ]->[ $x ];
        last unless $tile;
        STDERR->print("laser firing $dir continues to $x, $y on a tile $tile\n");
        last if $dir eq 'e' and $tile =~ m/wall_[nse]*$opdir->{$dir}/ and ! $shoot_through_walls_flag;   # east bound laser (for example) hitting a west wall prevents the laser from entering this tile.  ignoring nse walls for the moment.
    }
}

sub simulate {
    my $step = shift or die;

    my @objects = grep $board->on_the_board($_), $board->cards;                              # objects on the board, including and especially robots

    STDERR->print("objects on board: " . join(', ', map $_->id, @objects), "\n");
    # map { STDERR->print("object @{[ $_->id ]} at @{[ $_->col ]}, @{[ $_->row ]}\n") } @$objects;

    # all robots take their moves in turn

    my @robot_turn_order = reverse sort { abs( $a->row - 8 ) + $a->col < abs( $b->row - 8 ) + $b->col  } grep $_->isa('robot'), @objects;
    $console->log("Register $step turn order: " . join(', ', map $_->id, @robot_turn_order) . "\n");

    for my $robot (@robot_turn_order) {

        # each robot takes a turn for the current register, sorted by distance from 0, 6 or so

        my $player = $robot->player or do {
            STDERR->print("robot priority dump: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, " has no player; skipping\n");
            next;
        };

        my @registers = $robot->player->registers;
        STDERR->print("registers for player @{[ $player->player_id ]}: " . join(', ', map $_ ? $_->front_img : '(undef)', @registers), "\n");

        my $card = $registers[$step] or do {
            STDERR->print("robot priority dump: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, ' player ', $player->player_id, " has no card in register $step; skipping\n");
            next;
        };

     drew_replacement_card:

        my $action = $card->card_action;

        STDERR->print($robot->name, ' at ', $robot->col, ', ', $robot->row, ' player ', $player->player_id, ' with card ', $card->id, " with action ${action}\n");
        $console->log("@{[ $robot->name ]} turns over a '$action' card.\n");
        
        if( $action eq 'card-again' or $action eq 'special-repeatroutine' ) {
            if( $step == 1 or ! $registers[$step-1] ) {
                STDERR->print("sim: ", $robot->name, " executed an 'again' card but has no card in the previous register slot; skipping\n");
                next;
            }
            $card = $registers[$step-1];
            $action = $card->card_action;
        }

        my $move_num = 0;   # by default, not moving any number of tiles

        my $orientation;
        my $x_delta;
        my $y_delta;

        my $update_movement_deltas = sub {
            # run this after rotating the robot
            $orientation = $robot->orientation or die "no orientation";  # n, s, w, or e
            # where we'll move relative our current location if we move one square the direction we are facing
            ($x_delta, $y_delta) = $deltas->{$orientation}->@*;
            STDERR->print("orientation: ``$orientation'' x_delta = $x_delta y_delta = $y_delta\n");
        };
        $update_movement_deltas->();

        if( $action eq 'back' ) {
            $move_num = 1;
            $x_delta = - $x_delta;
            $y_delta = - $y_delta;
        } elsif( $action eq 'left' ) {
            $robot->rotate_left;
            push @actions, action->new( type => 2, card => $robot, player_id => 'simulate', );   # rotate
            STDERR->print("rotate left card\n");
            $update_movement_deltas->();   # we aren't going to initiate another move if the movement card is a rotate card, so this probably isn't needed, but doesn't hurt
        } elsif( $action eq 'right' ) {
            $robot->rotate_right;
            STDERR->print("rotate right card\n");
            push @actions, action->new( type => 2, card => $robot, player_id => 'simulate', );   # rotate
            $update_movement_deltas->();   # as above
        } elsif( $action eq 'uturn' ) {
            $robot->rotate_left for 1..2;
            STDERR->print("u-turn card\n");
            push @actions, action->new( type => 2, card => $robot, player_id => 'simulate', );   # rotate
            $update_movement_deltas->();   # as above
        } elsif( $action eq 'powerup' or $action eq 'special-energyroutine') {
            $player->add_energy;
        } elsif( $action eq 'move1' ) {
            $move_num = 1;
        } elsif( $action eq 'move2' ) {
            $move_num = 2;
        } elsif( $action eq 'move3' or $action eq 'special-speedroutine') {
            $move_num = 3;
        } elsif( $action eq 'spam' ) {
            ($card) = sort { $b->z <=> $a->z } grep { $_->x >= $deck_x-10 and $_->x <= $deck_x+10 and $_->y >= $deck_y-10 and $_->y <= $deck_y+10 } $player->cards;
            if( ! $card) {
                $console->log("Attempted to a draw a card to run for the spam card, but no cards found in the draw pile for player @{[ $player->player_id ]}.\n");
                next;
            } else {
                # put the card in the discard pile but face up
                # XXX would be nice if all moves just bumped z but failing that, this should go to the top of the discard
                $card->x = $discard_x+5;  $card->y = $discard_y + 5;   # slightly offset from the main discard location
                push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'spam card drew to discard area');
                if( $card->hid ) {
                    $card->hid = 0;    # face up
                    push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
                }
                goto drew_replacement_card;
            }
        } else {
            STDERR->print("don't recognize the ``$action'' action card\n"); # shouldn't happen as only cards in $player->cards are permitted in that player's registers
            next;
        }

        $robot->speed = $move_num;

        for( my $move_index=0; $move_index < $move_num; $move_index++ ) {
            my $res = recursive_move($robot, $x_delta, $y_delta, \@objects);
            STDERR->print("attemped move $move_index of $move_num in $orientation with result $res\n");
        }

    }

    # conveyer board actions for tiles the robots are left standing on after moving and pushing

    my @convey_queue = grep { $board->tiles->[ $_->row ]->[ $_->col ] =~ m/convey/ } @objects; 
    my $convey_tries = 0;
    while ( @convey_queue ) {
         my $robot = shift @convey_queue;

        STDERR->print("conveyer robot board actions: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, "\n");

        my $handle_conveyer_rotate = sub {
            my $robot = shift;
            # robots rotate on conveyer turns only if they were moved there along the conveyer, so this is invoked by conveyer move routines to check the new tile the player landed on and rotate them if needed
            my $tile = $board->tiles->[ $robot->row ]->[ $robot->col ];
            return unless $tile =~ m/convey/;
            (my $rotation) = $tile =~ m/_([nswe][nswe])$/ or return;
            if( $rotation eq 'ne' or $rotation eq 'wn' or $rotation eq 'sw' or $rotation eq 'es') {
                # rotations where the robot takes a right
                $console->log("Conveyer $rotation rotated @{[ $robot->name ]} right.\n") if $robot->isa('robot');
                $robot->rotate_right;
            } elsif( $rotation eq 'nw' or $rotation eq 'ws' or $rotation eq 'se' or $rotation eq 'en') {
                # rotations where the robot takes a left
                $console->log("Conveyer $rotation rotated @{[ $robot->name ]} left.\n") if $robot->isa('robot');
                $robot->rotate_left;
            } else {
                die "unrecognized rotation ``$rotation''";
            }
            push @actions, action->new( type => 2, card => $robot, player_id => 'simulate/conveyer', );   # rotate
        };

        my $tile = $board->tiles->[ $robot->row ]->[ $robot->col ];     # XXX and we should consider objects placed here but we'd have to dig through items including our own bot at least.  so guess the board is just mutible instead of modifyable with tiles placed on top, for the time being.

        my $any_double_conveyer_ran_twice = 0;

      run_double_conveyer_a_second_time:
        if( $tile =~ m/convey_.*/ ) {
            # there are 8 possible straight segments, one for each cardinal direction with two possible directions each.
            # there are 8 possible turns... ne nw se sw wn ws en es.
            # we only care about the direction they exit, whether we need to repeat for a double speed conveyer, and rotating when conveying on to a turn.
            # XXX one complication is robots can be pushed off the conveyer if the robot behind them conveyes first, which doesn't make real-world sense.  
            # XXX maybe bots without bots in front (in the sense of the conveyer direction, not bot facing) of them should go first?  how it is now is non-deterministic.
            # XXX another gotcha here is if we try and fail to push a robot and we're on a corner on a conveyer, we could be rotated multiple times.
            # XXX one strategy would be to work left to right in moving robots on w moving conveys, etc, for each of the four directions... oof, but then going around a corner, you could get done twice
            (my $dir) = $tile =~ m/convey_.*([nswe])$/ or die "invalid dir on the end of ``$tile''"; 
            STDERR->print("conveyer moving ``$dir''\n");
            (my $x_delta, my $y_delta) = $deltas->{$dir}->@*;
            # try to move the robot along the conveyer
            if( recursive_move($robot, $x_delta, $y_delta, \@objects, 1) ) {
                $console->log("@{[ $robot->name ]} moved $dir by conveyer.\n");
            } else {
                if( $convey_tries++ > 10 ) {
                    $console->log("Giving up on conveying objects after 10 attempts to postpone conveying an object to try to break deadlock without luck.\n");
                    last;
                }
                STDERR->print("convey: we'd push another robot if we did that so trying @{[ $robot->name ]} again later.\n");
                push @convey_queue, $robot;  # try again after trying the other robots
                next;
            }
            $handle_conveyer_rotate->($robot);
            if( $tile =~ m/_dbl_/ and ! ++$any_double_conveyer_ran_twice ) { STDERR->print("conveyer repeats\n"); goto run_double_conveyer_a_second_time; }
        }
    }

    # power up, rotate board actions

    for my $robot ( grep $_->isa('robot'), @objects ) {

        STDERR->print("other robot board actions: ", $robot->name, ' at ', $robot->col, ', ', $robot->row, "\n");

        my $tile = $board->tiles->[ $robot->row ]->[ $robot->col ];

        if( $tile eq 'power' and $step == 5 and $robot->player ) {
            $console->log("@{[ $robot->name ]} collects power from a power square at the end of register 5.\n");
            $robot->player->add_energy;
        } elsif( $tile eq 'rotate_left' ) {
            $console->log("@{[ $robot->name ]} is rotated left by a gear.\n");
            $robot->rotate_left;
            push @actions, action->new( type => 2, card => $robot, player_id => 'simulate', );   # rotate
        } elsif( $tile eq 'rotate_right' ) {
            $console->log("@{[ $robot->name ]} is rotated right by a gear.\n");
            $robot->rotate_right;
            push @actions, action->new( type => 2, card => $robot, player_id => 'simulate', );   # rotate
        }
    }

    # board lasers

    for my $loop_x (0..$board->width_tiles_minus_one) {
        for my $loop_y (0..$board->height_tiles_minus_one) {
            my $x = $loop_x;
            my $y = $loop_y;
            my $tile = $board->tiles->[ $y ]->[ $x ];
            next unless $tile =~ m/laser/;
            (my $dir) = $tile =~ m/_([nswe])$/ or do { STDERR->print("failed to find direction on laser ``$tile''\n") };
            $dir = $opdir->{$dir};            # walls shoot lasers the opposite direction from where they are on a tile so eg a wall on the south of the tile shoots lasers north
            # XXX board double lasers using the $double_barrel_lasers flag
            lasers($board, $dir, $x, $y, \@objects, 0, $step);
        }
    }

    # robot lasers
           
    for my $robot ( grep $_->isa('robot'), @objects ) {
        my $dir = $robot->orientation or die "no orientation";  # n, s, w, or e
        my $x = $robot->col;
        my $y = $robot->row;

        my $shoot_through_walls = $robot->player && $robot->player->has_upgrade('upgrade-railgun');
        my $double_barrel_lasers = $robot->player && $robot->player->has_upgrade('upgrade-doublebarrellaser');

        # presser beam
        if( $robot->player and $robot->player->has_upgrade('upgrade-pressorbeam') and $robot->player->presser_beam_counter++ >= 2 ) {
            # fires with every third laser
            $robot->player->presser_beam_counter = 0;
            STDERR->print("robot presser beam from @{[ $robot->name ]} firing $dir starting at $x, $y\n");
            lasers($robot, $dir, $x, $y, \@objects, 1, $step, 0, 0, sub {
                # lasers() does:   $missile_callback->($robot, $dir, $objects);
                my $obj = shift;     # object we hit
                my $dir = shift;     # direction of projectile, which also happens to be the direction we want to move them
                my $objects = shift;   # XXX board objects should probably just be global, or $board->objects, or something
                (my $x_delta, my $y_delta) = $deltas->{$dir}->@*;
                my $res = recursive_move($obj, $x_delta, $y_delta, $objects, 0);      # recursive_move() recursively pushes according to usual push rules; not sure if that's as intended
                $console->log($robot->name . ' hit ' . $obj->name . " with its presser beam!\n") if $res;
            });
        }

        # tractor beam
        if( $robot->player and $robot->player->has_upgrade('upgrade-tractorbeam') and $robot->player->tractor_beam_counter++ >= 1 ) {
            # fires with every other laser
            $robot->player->presser_beam_counter = 0;
            STDERR->print("robot tractor beam from @{[ $robot->name ]} firing $dir starting at $x, $y\n");
            lasers($robot, $dir, $x, $y, \@objects, 1, $step, 0, 0, sub {
                # lasers() does:   $missile_callback->($robot, $dir, $objects);
                my $obj = shift;     # object we hit
                my $dir = shift;     # direction of projectile, which also happens to be the direction we want to move them
                my $objects = shift;
                (my $x_delta, my $y_delta) = $deltas->{$opdir->{$dir}}->@*;            # opposite dir from direction of projectile movement
                my $res = recursive_move($obj, $x_delta, $y_delta, $objects, 1);      # unlike the presserbeam, does not recursively move robots, which satisfies the rule that you can't pull a robot standing right next to you
                $console->log($robot->name . ' hit ' . $obj->name . " with its tractor beam!\n") if $res;
            });
        }

        # lasers
        STDERR->print("robot laser from @{[ $robot->name ]} firing $dir starting at $x, $y\n");
        lasers($robot, $dir, $x, $y, \@objects, 1, $step, $shoot_through_walls, $double_barrel_lasers);

         # rear lasers
        if( $robot->player and $robot->player->has_upgrade('upgrade-rearlaser') ) {
            STDERR->print("robot laser firing rear lasers $dir starting at $x, $y\n");
            lasers($robot, $opdir->{$dir}, $x, $y, \@objects, 1, $step);   # not applying $shoot_through_walls, $double_barrel_laser to rear facing lasers
        }
    }

    # items on this space

    for my $robot ( grep $_->isa('robot'), @objects ) {
        (my @obj) = grep { ! $_->isa('robot') and $_->col == $robot->col and $_->row == $robot->row } @objects;
        for my $obj (@obj) {
            STDERR->print("robot @{[ $robot->name ]} stepped on a @{[ $obj->id ]}\n");
            $obj->standing_on($robot);
        }
        
    }

    # crates
    if( $crate_counter++ > 3 ) {
        $crate_counter = 0;
        my $crate = $crates[ $last_crate_deployed++ ];
        STDERR->print("deploying a crate:  @{[ $crate->id ]}\n");
        if($crate) {
            if( $last_crate_deployed % 2 ) {
                $crate->col = 12;
                $crate->row = 0;
            } else {
                $crate->col = 2;
                $crate->row = 0;
            }
            push @actions, action->new( type => 1, card => $crate, player_id => 'donkey kong', text => 'deploy crate');  # move
        }
    }

    # XXX push panels, crushers

}

#                                   .__            __   
#   ______ ____ _____  ______  _____|  |__   _____/  |_ 
#  /  ___//    \\__  \ \____ \/  ___/  |  \ /  _ \   __\
#  \___ \|   |  \/ __ \|  |_> >___ \|   Y  (  <_> )  |  
# /____  >___|  (____  /   __/____  >___|  /\____/|__|  
#      \/     \/     \/|__|       \/     \/  

# write a png of the board

sub snapshot {
    my $phase = shift;
    try {
        my $board_img = $board->png->clone;
        my @objects = grep $board->on_the_board($_), $board->cards;                              # objects on the board, including and especially robots
        for my $obj (sort { $a->z <=> $b->z } @objects) {
            $board_img->blit($obj->png, 69 * $obj->col, 69 * $obj->row);
        }
        open my $imgfh, '>', "$main::Bin/jpg/phase_$phase.png" or die $!;
        $imgfh->print( $board_img->write );
        close $imgfh;
        # $console->log(qq{<a href="/jpg/phase_$phase.png" target="_blank">Phase $phase board image.</a>});  # XXX have to fix escaping/quoting first
    } catch {
        warn "generating a board image: $_\n";
    };
}


#                  __  .__                      
#   _____    _____/  |_|__| ____   ____   ______
#   \__  \ _/ ___\   __\  |/  _ \ /    \ /  ___/
#    / __ \\  \___|  | |  (  <_> )   |  \\___ \ 
#   (____  /\___  >__| |__|\____/|___|  /____  >
#        \/     \/                    \/     \/ 

sub login {
    my $request = shift;
    my $greeting;

    login_screen:
    ($greeting) = map $_->[rand @{$_}], [
        "dear fellow",
        "sport",
    ];
    $request->print(qq{
        Okay, do be a $greeting and tell me who you are.<br>
        <form method="post">
            <input type="text" name="player_id"> &lt;-- name <br>
            <input type="password" name="password"> &lt;-- password (make one up to create an account, accounts are very transient)<br>
            <input type="password" name="gamepass"> &lt;-- password to join the game<br>
            <input type="submit" value="Go">
    });

    # check password

    $request = $request->next;

    my $player_id = $request->param('player_id') or do { $request->print("No name entered...<br>\n"); goto login_screen; };
    my $password = $request->param('password') or do { $request->print("No password entered...<br>\n"); goto login_screen; };
    $request->param('gamepass') eq 'wee' or do { $request->print("No gamepass entered or wrong gamepass...<br>\n"); goto login_screen; };

    (my $illegal) = $player_id =~ m{([^a-zA-Z0-9 -])};
    if(defined $illegal) {
        $illegal = '&' . ord($illegal) . ';';
        $request->print(qq{Character $illegal is forbidden (or, more correctly, not explicitly permitted) for use in names.<br>\n});
        goto login_screen;
    }

    (my $player) = grep $_->player_id eq $player_id, @players;
    if(! $player) {
        $player = player->new( player_id => $player_id, password => $password );
        push @players, $player;
        $request->print(qq{Welcome, new player $player_id.<br>\n});
    } elsif( $player->password eq $password ) {
        $request->print(qq{Welcome back, $player_id.<br>\n});
    } else {
        $request->print(qq{Sorry, $player_id exists and that wasn't the right password.<br>\n});
        goto login_screen;
    }

    push @actions, action->new( type => 0, text => "logs in", player_id => $player_id, );

    $request->print(qq{
        <br>
        <form method="post" action="$url/board"><input type="hidden" name="player_id" value="$player_id"><input type="hidden" name="password" value="$password"><input type="submit" value="Enter game"><br>
    });

    return $player;

}

sub board {
    # redraw the board
    # send the game board
    # go through all of the cards and output HTML for them
    my $request = shift;
    my $generation = scalar(@actions) || 0;
    my $player = shift;
    my $player_id = $player->player_id;
    my $password = $player->password;     # auto-login on our first request to /updator (and also subsequent requests even though they don't look for it)
    $request->print(<<EOF);

        <head>
        </head>
        <body bgcolor="white">

            <script language="javascript" src="/dom-drag.js"></script>
            <script language="javascript" src="/ajax.js"></script>

            <div id="sync">Sync: </div>
             <!-- <a href="$url/board?action=game_log" target="gamelogwin">game log</a> / <a href="$url/board?action=reset">reset game</a> -->
            <table width="$board->{board_x}" height="$board->{board_y}"><tr><td width="$board->{board_x}" height="$board->{board_y}">   <!-- XX could probably do this with a div instead of a table -->

            <!-- ajax glue -->
            <script language="javascript">

                function do_request2(querystring) {
                    // XXX would be better to use the Drag.js API instead of having this shim
                    do_request( '$url/board' + querystring + '&action=move&player_id=$player_id&password=$password', function () { } );  // querystring has its own ? as it happens... should clean this up XX
                }

                var generation = $generation;

                function call_for_updates () {
                    // console.log('calling for updates');

                    do_request(
                        '$url/updator?player_id=$player_id&password=$password&action=updator&generation=' + generation, 
                        function (page) {

                            // console.log('got updates: ' + page);

                            // schedule another call_for_updates() call regardless of whether the server returned an error, and before we can stumble over any execution errors
                            window.setTimeout(
                               function () { call_for_updates() },
                               1000  // 1 second
                            );

                            page = JSON.parse(page);

                            generation = page.sync;
                            document.getElementById('sync').innerHTML = "Sync: " + page.sync;

                            page.actions.forEach( function (action) {

                                // console.log('doing update command: ' + action.command); console.log(action);

                                var el = document.getElementById(action.id);

                                if(action.command == 1) {
                                    // move a card
                                    el.root.style.left = action.x + 'px';
                                    el.root.style.top = action.y + 'px';
                                    el.root.style.zIndex = action.z;
                                    el.root.style.display = 'block';   // also show the card if it was previously hidden, though another hide command might be right after this move XXX would have to detect when a card is moved off an area to not have to do this
                                } else if(action.command == 2) {
                                    // rotate
                                    if(action.width) el.root.style.width = action.width + 'px';
                                    if(action.height) el.root.style.height = action.height + 'px';
                                    if(action.image) el.root.src = action.image;
                                    // el.root.style.rotate = action.rotation + 'deg'; // works in Firefox but not Chrome
                                    el.root.style.transform = 'rotate(' + action.rotation + 'deg)';
                                } else if(action.command == 3) {
                                    // flip a card over for all to see
                                    el.root.src = action.image;
                                } else if(action.command == 4) {
                                    // peek
                                    // XXX dhtml would be better, and this likely got mangled in translation anyway.  also, height and width should just come from the browser side img object.
                                    //        window.open(action.img, action.id, "innerWidth="+action.width+",innerHeight="+action.height);
                                } else if(action.command == 5) {
                                    // dice roll
                                    el.root.style.left = action.x + 'px';
                                    el.root.style.top = action.y + 'px';
                                    el.root.src = action.image;
                                } else if(action.command == 6) {
                                    // add border to a card XXXXXXXXX testing
                                    el.root.style.border = '2px solid red';
                                } else if(action.command == 7) {
                                    // remove border from a card
                                    el.root.style.border = '0px solid red';
                                } else if(action.command == 8) {
                                    // hide card
                                    el.root.style.display = 'none';
                                } else if(action.command == 9) {
                                    // change z as part of reshuffling
                                    el.root.style.zIndex = action.z;
                                } else if(action.command == 10) {
                                    el.append(action.text);
                                    el.append( document.createElement('br') );
                                    el.scrollTop = el.scrollHeight;
                                }

                            }); // end each action

                        } // end AJAX callback
                   ); // end do_request() call for updates

                };


                window.onload = call_for_updates;

            </script>
            <!-- game pieces -->
EOF
    # <script language="javascript" src="dom-drag.js"></script>
    # <img id="navro" onload="Drag.init(this);" src="images/brickwallRO.jpg" style="position:absolute; left: 621px; top: 10px; z-index: 1000;">
    for my $card ($board->cards) {
        # XXX would be nice to use the Drag.js API instead of the modification to it to make it call do_request2()
        if( my $html = $card->html ) {
            # XXX this case should be the only case and the other case should move to an html() method in the card base class
            $request->print($html);
        } else {
            my $show_only =  $card->show_only ? $card->show_only ne $player_id ? 'display: none;' : '' : '';   # refreshing the page still leaks the position of cards marked display: none; should move them to 0, 0 also in this case
            my $highlighted = $card->highlighted ? $card->highlighted eq $player_id ? "border: 2px solid red;" : '' : '';
            my $rotation = $card->rotation ? 'transform: rotate(' . $card->rotation . 'deg);' : '';
            $request->print(qq{
                <img id="$card->{id}" alt="$card->{id}" src="@{[ $card->image ]}" style="position:absolute; left: $card->{x}px; top: $card->{y}px; z-index: $card->{z}; width: $card->{card_width}px; height: $card->{card_height}px; $show_only $highlighted $rotation" @{[ $card->stationary ? '' : q<onload="Drag.init(this);"> ]} >
            });
        }
    }
    $request->print(qq{
            </td></tr></table>
        </body>
    });
}

sub move {
    # move a card
    # this action is called from the JavaScript output by board() above
    my $request = shift;
    my $player = shift;
    my $player_id = $player->player_id;

    my $id = $request->param('id');
    my $x = $request->param('x');
    my $y = $request->param('y');
    my $card = $board->card_by_id($id);
    STDERR->print("move request: id: $id x: $x y: $y\n");
    $card->x = $x; $card->y = $y;

    # push @actions, action->new( type => 1, card => $card, player_id => $player_id, text => $card->x . ', ' . $card->y);  # XXX testing
    # card moved actions (dice, etc)

    $card->moved($player); 

    # card movement action (dice could and should be done this way)

    # overlap actions

    for my $indirect_card ($board->cards) {
        if($card->overlaps( $indirect_card )) {
            $indirect_card->dropped_on_us($card, $player);
        }
        
    }

    $request->print("ok\n");  # or else AJAX doesn't work when there's no output
}

sub updator {
    # check for movement and send commands to update the playfield
    my $request = shift;
    my $player = shift;
    my $player_id = $player->player_id;
    my $generation = $request->param('generation');
    if($poll_time) {
        # dally on responding to the request until $poll_time seconds have passed or something changes
        my $timer = Coro::Event->timer( interval => 1, );
        my $give_up = $poll_time;
        while($generation == @actions and $give_up-- > 0) {
            # STDERR->print("debug: $con_num sleeping...\n");
            $timer->next;
        }
    }

    my @new_actions;   # data reformatted and simplfied vs @actions

    while($generation < @actions) {
        my $action = $actions[$generation];
        # 0 -- comment, 1 -- move, 2 -- rotate , 3 -- flip card, 4 -- reveal, 5 -- roll dice
        if($action->type == 1 and $action->player_id ne $player_id and ( ! $action->card->show_only or $action->card->show_only eq $player_id ) ) {
            # move a card
            # don't repeat move requests to people who initiated them as it just creates a race that makes them have trouble moving it again
            # XXX should increase Z in this case so the card lands on top of anything it is dropped on
            push @new_actions, { id => $action->card->id, command => 1, x => $action->card->x, y => $action->card->y, z => $action->card->z, };
        } elsif($action->type == 2) {
            # rotation
            push @new_actions, { id => $action->card->id, command => 2, image => $action->card->image, height => $action->card->card_height, width => $action->card->card_width, rotation => $action->card->rotation, };
        } elsif($action->type == 3) {
            # flip a card over for all to see
            push @new_actions, { id => $action->card->id, command => 3, image => $action->card->image };
        } elsif($action->type == 4) {
            STDERR->print("debug: peeking at a card: $player_id vs @{[ $action->player_id ]}\n");
            if( $action->card->player_id eq $player_id ) {
                # reveal a card to its owner only
                push @new_actions, { id => $action->card->id, command => 4, image => $action->card->image };
            }
        } elsif($action->type == 5) {
            # dice roll
            push @new_actions, { id => $action->card->id, command => 5, x => $action->card->x, y => $action->card->y, image => $action->card->image };
        } elsif($action->type == 6) {
            # add a red border only for $card->player_id
            if( $action->player_id eq $player_id ) {
                push @new_actions, { id => $action->card->id, command => 6, };
            }
        } elsif($action->type == 7) {
            # remove border
            push @new_actions, { id => $action->card->id, command => 7, };
        } elsif($action->type == 8 and $action->card->show_only and $action->card->show_only ne $player_id ) {
            # hide the player for everyone except the player specified
            # $card->show_only is required but since that's shared data, it's possible that it will be deleted again by the time this happens so gracefully handle that having been cleared
            # XXX kinda should just have one action queue per player.
            push @new_actions, { id => $action->card->id, command => 8, };
        } elsif($action->type == 9) {
            # change card z for re-shuffling
            push @new_actions, { id => $action->card->id, command => 9, z => $action->card->z, };
        } elsif($action->type == 10) {
            # text console broadcast message
            push @new_actions, { id => $action->card->id, command => 10, text => $action->text, };
        }
    } continue {
        $generation++;
    }

    $request->print(encode_json({ success => \1, sync => scalar(@actions), actions => \@new_actions, }));

}

sub game_log {
    # display a transcript of everything that has happened so far -- the main game board would
    # probably link to this in a pop-up window
    my $request = shift;
    my $player = shift;
    my $player_id = $player->player_id;
    $request->print("<b>Game log, actions in reverse order:</b><br>\n");
    for my $action (reverse @actions) {
        $request->print( $action->desc, "<br>\n" );
    }
}

sub reset {
    my $request = shift;
    my $player = shift;
    my $player_id = $player->player_id;
    # $shuffle_cards->();    # re-shuffling needs a lot of work
    board( $request, $player ); 
}

#           __  .__.__  .__  __  .__               
#    __ ___/  |_|__|  | |__|/  |_|__| ____   ______
#   |  |  \   __\  |  | |  \   __\  |/ __ \ /  ___/
#   |  |  /|  | |  |  |_|  ||  | |  \  ___/ \___ \ 
#   |____/ |__| |__|____/__||__| |__|\___  >____  >
#                                        \/     \/ 

sub escape {
    my $value = shift;
    $value =~ s{([^a-zA-Z0-9])}{ '%' . sprintf '%02x', ord $1 }sge;
    return $value;
}

#                __  .__               
# _____    _____/  |_|__| ____   ____  
# \__  \ _/ ___\   __\  |/  _ \ /    \ 
#  / __ \\  \___|  | |  (  <_> )   |  \
# (____  /\___  >__| |__|\____/|___|  /
#      \/     \/                    \/

package action;

sub new { my $pack = shift; bless { @_ }, $pack; }
sub type :lvalue { $_[0]->{type} }  # 0 -- comment, 1 -- move, 2 -- rotation, 3 -- img update (faceup/down), 4 -- reveal, 5 -- roll
sub card :lvalue { $_[0]->{card} }  # reference to the card that's been moved, revealed, or whatever
sub text :lvalue { $_[0]->{text} }  # comment on the move
sub player_id :lvalue { $_[0]->{player_id} } # originating player's id
sub desc {
    my $action = shift;
    my $desc = '';
    $desc .= "<i>" . $action->player_id . "</i> ";
    if($action->type == 1) {
        $desc .= "moved the " . $action->card->id . " card"; #  to position (", $action->card->x, ', ', $action->card->y, ")"); # no, we don't know what position it was moved to, only where it's at now
    } elsif($action->type == 2) {
        # $request->print("rotated the ", $action->card->id, " card"); # not really even interesting enough to log
    } elsif($action->type == 3) {
        $desc .= "flipped over the " . $action->card->id . " card";
    } elsif($action->type == 4) {
        $desc .= "peeked at the " . $action->card->id . " card";
    } elsif($action->type == 5) {
        $desc .= "rolled a " . $action->text;
    }
    if($action->text) {
        $desc .= '; ' if $desc;
        $desc .= $action->text;
    }
    return $desc;
}

#        ___.        __               __          
#    ____\_ |__     |__| ____   _____/  |_  ______
#   /  _ \| __ \    |  |/ __ \_/ ___\   __\/  ___/
#  (  <_> ) \_\ \   |  \  ___/\  \___|  |  \___ \ 
#   \____/|___  /\__|  |\___  >\___  >__| /____  >
#             \/\______|    \/     \/          \/ 

package card;

use Want;
use png;

sub new {
    my $pack = shift; 
    my $self = bless {
        orientation => 'n',
        @_,
     }, $pack; 
    if( $self->front_img and ( ! $self->card_width or ! $self->card_height ) ) {
         my $fn = $main::Bin . $self->front_img;
         ( $self->card_width, $self->card_height ) = Image::Info::dim( Image::Info::image_info( $fn ) );
         # STDERR->print("detected card size of @{[ $self->card_width ]} x @{[ $self->card_height ]}\n"); 
         STDERR->print("debug: failed to detect height/width for fn $fn id $self->{id}\n") unless $self->card_width and $self->card_height;
    }
    return $self;
}
sub x :lvalue { $_[0]->{x} }         # raw x, y and layer depth the graphic is rendered to on the screen
sub y :lvalue { $_[0]->{y} } 
sub z :lvalue { $_[0]->{z} } 
sub col :lvalue {            # the computed row and col relative to the game board regardless of whether we are actually on it, and handle writes.  actual position is stored in x; this translates to/from that. 
    my $self = shift;
    if( want(qw'LVALUE ASSIGN') ) {
        my $col = want('ASSIGN');
        $self->{col} = $col;
        $self->{x} = $board->x + 69 * $col;
        lnoreturn;
    } else {
        my $x = $self->x - $board->x;  $x /= 69 if $x; $x = int( $x + 0.3 );
        return $x;
    }
    return;
}
sub row :lvalue {           # as above, update y from a board row specified, or else convert our y to a row on the board
    my $self = shift;
    if( want(qw'LVALUE ASSIGN') ) {
        my $row = want('ASSIGN');
        $self->{row} = $row;
        $self->{y} = $board->y + 69 * $row;
        lnoreturn;
    } else {
        my $y = $self->y - $board->y;  $y /= 69 if $y; $y = int( $y + 0.3 );
        return $y;
    }
    return;
} 
sub colrow {
    my $card = shift; my $col = shift; my $row = shift; $card->col = $col; $card->row = $row; 
    push @actions, action->new( type => 1, card => $card, player_id => 'system', text => "col $col, row $row");   # move
}
sub xy {
    my $card = shift; my $x = shift; my $y = shift; $card->x = $x; $card->y = $y; 
    push @actions, action->new( type => 1, card => $card, player_id => 'system', text => "$x, $y");   # move
}
sub card_width :lvalue { $_[0]->{card_width} }
sub card_height :lvalue { $_[0]->{card_height} }
sub show_only :lvalue { $_[0]->{show_only} }          # only show this card to a specific player
sub highlighted :lvalue { $_[0]->{highlighted} }   # player_id of player it is highlighted for
sub id :lvalue { $_[0]->{id} }
sub image { $_[0]->hid ? $_[0]->{back_img} : $_[0]->{front_img} }
sub front_img :lvalue { $_[0]->{front_img} }
sub back_img :lvalue { $_[0]->{back_img} }
sub card_action { my $card = shift; (my $action = $card->front_img) =~ s{.*/card[_-](.*?)\.(png|jpg)$}{$1}; $action; }
sub hid :lvalue { $_[0]->{hid} }                      # flipped face down
sub stationary { $_[0]->{stationary} || 0 }
sub dropped_on_us { }   # function to run when something is dropped overlapping this tile/card/pad, to be defined in a subclass
sub html {  }           # render us when the board is first drawn; XXX I guess HTML should be moved here for cards but for the moment, subclasses use this to override default HTML
sub orientation :lvalue { 
    my $self = shift;
    my $dir = want(qw'LVALUE ASSIGN') ? want('ASSIGN') : @_ ? $_[0] : undef;
    if($dir) {
        if( $dir eq 'north' or $dir eq 'n' ) {
            $self->{orientation} = 'n';
        } elsif( $dir eq 'south' or $dir eq 's' ) {
            $self->{orientation} = 's';
        } elsif( $dir eq 'west' or $dir eq 'w' ) {
            $self->{orientation} = 'w';
        } elsif( $dir eq 'east' or $dir eq 'e' ) {
            $self->{orientation} = 'e';
        } else {
            die "invalid orientation: $dir";
        }
        lnoreturn;
    }
    return $self->{orientation};
}
sub rotation { { n => 0, w => 270, e => 90, s => 180, }->{ $_[0]->{orientation}//0 }; }
sub rotate_right {
    my $self = shift; 
    $self->orientation = {
        n => 'e',
        e => 's',
        s => 'w',
        w => 'n',
    }->{ $self->orientation };
}
sub rotate_left {
    my $self = shift; 
    $self->orientation = {
        n => 'w',
        w => 's',
        s => 'e',
        e => 'n',
    }->{ $self->orientation };
}
sub moved {
    # function to run when we are moved
    my $card = shift;
    my $player = shift;
    # the player who moved the card already has it moved, and we have the new location in the database.
    # either send a move or hide event for other players depending on whether it landed on the player's private pad or not.
    # all player mats are in the same physical space on the board, but $player is the person who moved us.
    if( $card->overlaps( $player ) and ! $card->show_only ) {
        $card->show_only = $player->player_id;
        push @actions, action->new( type => 8, card => $card, player_id => $player->player_id, text => $card->x . ', ' . $card->y);   # hide the card for everyone else
    } else {
        # $card->show_only = undef; # nope... used to be the case that if we moved something off our pad, it should become public again, but now our move deck stays private no matter what.  have to explicitly delete show_only if we want to make the card visible again, and also, that won't work for players when the card was created after they first loaded the board because we don't push object creation.
        # XXX may need to add logic to make spam cards public again after being moved off our board, but if there are enough of them, it shouldn't actually hurt anything not to.  or during reset, spam cards are made unprivate en masse.
        push @actions, action->new( type => 1, card => $card, player_id => $player->player_id, text => $card->x . ', ' . $card->y);   # move the card for everyone else
    }
}
sub overlaps {
    # true if dest_card is approximately on top of us
    my $card = shift;             # card that got moved; genearlly smaller
    my $dest_card = shift;        # possible bottom card... generally a mat or the game board or the like; generally larger
    if(
        $card->x > $dest_card->x - $card->card_width/4  and                                 # card can be 1/4th of the way off us (dest_card) to the left
        $card->x < $dest_card->x + $dest_card->card_width - $card->card_width/2  and        # card be 1/2 its width off of us to the right
        $card->y > $dest_card->y - $card->card_height/8 and                                 # card can be 1/8th off of us on the top
        $card->y < $dest_card->y + $dest_card->card_height - $card->card_height/2           # card can be 1/2 its height off us us on the bottom
    ) {
        return 1;
    } else {
        return 0;
    }
}
sub step_on { 1 }    # returns 1 to allow robots (or another object) to enter its space, 0 to deny
sub standing_on { }  # like step_on, but called at the end of a register instead of during movement
sub push_me { 1 }    # happens when we can't be stepped on (allowing both step_on and push_me doesn't make sense as step_on takes priority); returns 1 to allow robots (or other objects) to push us, 0 to deny
sub player { }
sub name {
    my $self = shift;
    if( $self->can('player') and $self->player ) {
        return $self->id . ' (' . $self->player->player_id . ')';
    } else {
        return $self->id;
    }
}
sub reload_png { my $self = shift; delete $self->{png}; $self->png; }
sub png {
    my $self = shift;
    delete $self->{png} if $self->{last_hid} and $self->{hid} != $self->{last_hid};    # reload data if we've been flipped over.  generally doesn't apply to objects on the board so we keep only one image buffer handy.
    $self->{last_hid} = $self->{hid};
    return $self->{png} if $self->{png};
    $self->{png} = png::read( $main::Bin . $self->image );  # all image paths have a leading / which is a slightly dubious system
    return $self->{png};
}

#

package spam;
use base 'card';

#

package checkpoint;
use base 'card';
sub step_on { 1 }         # can be stpped on (which means we won't be pushed)
sub checkpoint_num { $_[0]->{checkpoint_num} }
sub standing_on {
    my $self = shift;
    my $robot = shift;
    STDERR->print("@{[ $robot->name ]} is standing on checkpoint @{[ $self->checkpoint_num ]}.\n");
    return unless $robot->player;
    # make sure they're doing the checkpoints in sequence
    if( $robot->player->checkpoint - 1 == $self->checkpoint_num ) {
        $robot->player->checkpoint = $self->checkpoint_num;
        $console->log("@{[ $robot->name ]} cleared checkpoint @{[ $self->checkpoint_num ]}.\n");
    }
    return 1;   # allow robot to step on us
}
#

package energy;
use base 'card';
sub new { my $package = shift; my $self = $package->SUPER::new( front_img => '/jpg/energy.png', back_img => '/jpg/energy.png', ); $self; }
sub step_on { 1 }         # can be stpped on (which means we won't be pushed)
sub standing_on {
    my $self = shift;
    my $robot = shift;
    $console->log("@{[ $robot->name ]} stepped on an energy cube.\n");
    return unless $robot->player;
    $robot->player->add_energy($self);
    return 1;   # allow robot to step on us
}
sub energy_last_x :lvalue { $_[0]->{energy_last_x} }
sub energy_last_y :lvalue { $_[0]->{energy_last_y} }

#

package crate;
use base 'card';
sub new { my $package = shift; my $self = $package->SUPER::new( front_img => '/jpg/crate.png', back_img => '/jpg/crate.png', ); $self; }
sub step_on { 0 }    # returns 1 to allow robots (or another object) to enter its space, 0 to deny
sub standing_on { }  # like step_on, but called at the end of a register instead of during movement
sub push_me { 1 }    # returns 1 to allow robots (or other objects) to push us, 0 to deny

#

package upgrade;
use base 'card';
sub moved {
    # function to run when we are moved
    my $card = shift;
    my $player = shift;
    if( $card->hid ) {
        $card->hid = 0;
        push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
    }
}

#

package dice;
use base 'card';

sub new { my $package = shift; my $self = $package->SUPER::new(@_); $self->do_roll; $self; }
sub roll :lvalue { $_[0]->{roll} }  
# sub do_roll :lvalue { $_[0]->roll = 1 + int rand 6; $_[0]->roll }  
sub image { '/jpg/d' . $_[0]->roll . '.gif'; }
sub front_image { $_[0]->image }
sub back_image { $_[0]->image }
sub do_roll {
    my $self = shift;
    $self->roll = 1 + int rand 6;
}
sub moved {
    my $self = shift;
    my $player = shift;
    $self->do_roll;
    push @actions, action->new( type => 5, card => $self, player_id => $player->player_id, text => $self->roll, );  # roll, which includes an image change and a position update
    return $self->roll;
}


# ___.                          .___
# \_ |__   _________ _______  __| _/
#  | __ \ /  _ \__  \\_  __ \/ __ | 
#  | \_\ (  <_> ) __ \|  | \/ /_/ | 
#  |___  /\____(____  /__|  \____ | 
#      \/           \/           \/ 

package board;
use base 'card';

sub new { 
    my $package = shift; 
    return $package->SUPER::new(
        cards => [ ],                              # all of the game cards on the board
        board_x => 2000, board_y => 2000,          # total size of the board
        card_width => 0,                           # width of the last card added to the board, so that we can move clear of it if we want to while doing layout
        cur_x => 0, cur_y => 0, cur_layer => 1,    # current position on the board we're laying cards out at
        @_,                                        # user-supplied values override ours
    );
}
#    for my $method (qw/card_x board_x board_y cur_x cur_y cur_layer layout_x layout_y/) {
#        *{$method} = sub :lvalue { my $self = shift; $self->{$method}; };
#    }
sub board_x :lvalue { $_[0]->{board_x} }    # entire play area size including white background
sub board_y :lvalue { $_[0]->{board_y} }
sub cur_x :lvalue { $_[0]->{cur_x} }        # current layout position on board
sub cur_y :lvalue { $_[0]->{cur_y} }
sub cur_layer :lvalue { $_[0]->{cur_layer} }
sub last_card_width :lvalue { $_[0]->{last_card_width} } # width the last card added to the board, remembered for the sake of doing layout on the board
sub last_card_height:lvalue { $_[0]->{last_card_height} }  # height of largest card added since the last newline
sub add {
    # the board is drawn as a card but it is also the container for all other cards
    my $self = shift;
    my $card = shift;
    $card->id or $card->id = sprintf ref($card).'%03d', $self->cur_layer;
    if(! $card->card_width or ! $card->card_height ) {
        die; # cards must know their width/height and we won't cache it for them
    }
    if(! defined $card->x or ! defined $card->y ) {
        # warn "automatically positioning " . $card->id;
        $card->x = $board->cur_x; $card->y = $board->cur_y;
    }
    $card->z or do { $self->cur_layer++; $card->z = $self->cur_layer; }; # XXXXXXX do this instead of next_z()
    $self->last_card_width = $card->card_width;
    $self->last_card_height = ($self->last_card_height//0) > $card->card_height ? $self->last_card_height : $card->card_height;
    push @{ $self->{cards} }, $card; 
}
sub move_past {
    my $self = shift;
    $self->cur_x += $self->last_card_width + 10;
    $self->last_card_width = 0;   # move_past() again will now do nothing unless another card is first added
    if($self->cur_x + $self->last_card_width + 1 >= $self->board_x) {
        $self->new_line();
    }
    ($self->cur_x, $self->cur_y);
}
sub new_line {
    my $self = shift;
    $self->cur_y += $self->last_card_height + 3;
    $self->cur_x = 0; 
    $self->last_card_width = 0;
    $self->last_card_height = 0;
    ($self->cur_x, $self->cur_y);
}
sub cards { @{ $_[0]->{cards} } }
sub card_by_id { my $self = shift; my $id = shift; for my $card ( $self->cards ) { return $card if $card->id eq $id; } }
sub next_z { my $self = shift; (my $start_z) = List::Util::max(grep defined $_, map $_->z, $self->cards); return $start_z; }   # XXXXX redundant and conflicting with cur_layer()... use that instead
sub on_the_board {
    my $self = shift;
    my $card = shift;
    # STDERR->print("on_the_board: @{[ $card->name ]}:  @{[ $card->col ]} vs @{[ $self->width_tiles_minus_one ]}, @{[ $card->row ]} vs @{[ $self->height_tiles_minus_one ]}\n");
    return 1 if $card->col >= 0 and $card->col <= $self->width_tiles_minus_one and $card->row >= 0 and $card->row <= $self->height_tiles_minus_one;
}
sub tiles {
    # XXX kludge since we're using one big board image instead of individual tiles at the moment
    # note convey_dbl_es... it goes east, then south, where _se would go south then east.  using a convention of the direction it's going as it enters and the directoin it goes as it exits if/when different.
    # note laser_wall_e... the wall is on the e side of the tile, but fires west.
    # note wall_nw... there's no northwest, only north and west walls on the same time.
    # note spawn_wall_s... that's just annoying.
    return [
        [qw/spawn wall_s convey_s open open open wall_n open wall_n open open wall_n convey_dbl_s wall_n open open/, undef],
        [qw/open spawn convey_s open open convey_dbl_ne convey_dbl_e convey_dbl_e convey_dbl_es open open convey_dbl_ne convey_dbl_e convey_dbl_e convey_dbl_es open/, undef],
        [qw/wall_w wall_n convey_se convey_es wall_w convey_dbl_n rotate_right wall_s convey_dbl_s rotate_left open convey_dbl_n rotate_right open convey_dbl_s wall_e/, undef],
        [qw/open open spawn convey_s open convey_dbl_n power rotate_right convey_dbl_s wall_w laser_wall_e convey_dbl_n power rotate_right convey_dbl_s open/, undef],
        [qw/wall_nw open open convey_s wall_w convey_dbl_wn convey_dbl_w convey_dbl_w convey_dbl_sw open rotate_left convey_dbl_wn convey_dbl_w convey_dbl_w convey_dbl_sw wall_e/, undef],
        [qw/open open wall_s spawn_wall_s open open open open rotate_left open open open laser_wall_n rotate_left open open/, undef],
        [qw/open open wall_n spawn_wall_n open open rotate_left laser_wall_s open open open rotate_left open open open open/, undef],
        [qw/wall_sw open open convey_n wall_w convey_dbl_ne convey_dbl_e convey_dbl_e convey_dbl_es rotate_left open convey_dbl_ne convey_dbl_e convey_dbl_e convey_dbl_es wall_e/, undef],
        [qw/open open spawn convey_n open convey_dbl_n rotate_right power convey_dbl_s laser_wall_w wall_e convey_dbl_n rotate_right power convey_dbl_s open/, undef],
        [qw/wall_w wall_s convey_ne convey_en wall_w convey_dbl_n open rotate_right convey_dbl_s open rotate_left convey_dbl_n wall_n rotate_right convey_dbl_s wall_e/, undef],
        [qw/open spawn convey_n open open convey_dbl_wn convey_dbl_w convey_dbl_w convey_dbl_sw open open convey_dbl_wn convey_dbl_w convey_dbl_w convey_dbl_sw open/, undef],
        [qw/spawn wall_n convey_n open open open wall_s open wall_s open open wall_s open wall_s open open/, undef],
        [],   # +1 or -1 off the map will index this
    ];
}
sub width_tiles {  scalar($_[0]->tiles->[0]->@*) - 1 };   # num tiles wide, minus one for the undef trailing each row that serves as a boundary
sub height_tiles { scalar($_[0]->tiles->@*) - 1; };      # num tiles high, minus one for the undef trailing all rows that serves as a boundary
sub width_tiles_minus_one { $_[0]->width_tiles - 1 }    # for when we want to county from 0
sub height_tiles_minus_one { $_[0]->height_tiles - 1 }

#              ___.           __   
# _______  ____\_ |__   _____/  |_ 
# \_  __ \/  _ \| __ \ /  _ \   __\
#  |  | \(  <_> ) \_\ (  <_> )  |  
#  |__|   \____/|___  /\____/|__|  
#                   \/           

package robot;
use base 'card';

sub new { my $package = shift; my $self = $package->SUPER::new( orientation => 'e', @_, ); $self; }
sub player :lvalue { $_[0]->{player} } 
sub speed :lvalue { $_[0]->{speed} }  # 0/1/2/3 by last card
sub moved {
    my $card = shift;
    my $player = shift;
    # the first time we're moved, associate with the player who moved us, if they aren't already associatec with another bot
    $card->SUPER::moved($player);  # generate the move/hide action as usual 
    return if $card->player;  # this robot is already associated with a player 
    return if $player->robot; # this player already has a robot
    # attach us to them and vice versa
    $card->player = $player;  # mutual references; if this wasn't permenant, we'd want to weaken one of those links for gc
    $player->robot = $card;
    $console->log($card->id . " now belongs to " . $player->player_id . "\n");
    $card->highlighted = $player->player_id;
    push @actions, action->new( type => 6, card => $card, player_id => $player->player_id, );  # red border, shown to this player only
}
sub step_on { 0 }    # returns 1 to allow robots (or another object) to enter its space via movement routines, 0 to deny
sub push_me {
    my $self = shift;  # the pushee
    my $pusher = shift;
    if( $pusher->player and $pusher->player->has_upgrade('upgrade-ramminggear') ) {
        # XXX there's a push_me() method but not a we_are_trying_to_push_them() method so we do damage to ourself if they have ramming gear
        $console->log("@{[ $pusher->id ]} pushed @{[ $self->id ]} using ramming gear, dealing spam damage.\n");
        $self->player->take_spam_damage if $self->player;
    }
}

#                   .___      
# ___________     __| _/______
# \____ \__  \   / __ |/  ___/
# |  |_> > __ \_/ /_/ |\___ \ 
# |   __(____  /\____ /____  >
# |__|       \/      \/    \/ 

package pad;
use base 'card';

sub new { my $package = shift; my $self = $package->SUPER::new(@_); $self->id = $package; $self; }
sub pad_color :lvalue { $_[0]->{pad_color} } 
sub pad_text :lvalue { $_[0]->{pad_text} } 
sub html {
    my $self = shift;
    # XXX should rename card_width and card_height to just width and height
    return qq(
        <div id="@{[ $self->id ]}" style="position: absolute; width: @{[ $self->card_width ]}px; height: @{[ $self->card_height ]}px; left: @{[ $self->x ]}px; top: @{[ $self->y ]}px; z-index: 0; background-color: @{[ $self->pad_color ]};">
            @{[ $self->pad_text ]}
        </div>
    );
}

package turnoverpad;
use base 'pad';
sub new {
    my $package = shift; 
    my $self = $package->SUPER::new( id => $package, pad_color => 'blue', pad_text => 'Turn over card<br/>Or drop it on your mat', @_ ); 
    $self;
}
sub dropped_on_us {
    my $self = shift;
    my $card = shift;
    my $player = shift;
    # function to run when something is dropped overlapping this tile/card/pad, to be defined in a subclass
    # this operates on the card that was dropped on us, not on us.  there's no point in trying to turn the pad over.
    $card->hid = ! $card->hid;
    push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
}

package rotateleftpad;
use base 'pad';

sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'pink', pad_text => 'Rotate left', @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift;
    my $player = shift;
    if( $card->hid ) {
        $card->hid = 0;
        push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
    }
    $card->rotate_left;
    push @actions, action->new( type => 2, card => $card, player_id => $player->player_id, );   # rotate
}

package rotaterightpad;
use base 'pad';

sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'pink', pad_text => 'Rotate right', @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift;
    my $player = shift;
    if( $card->hid ) {
        $card->hid = 0;
        push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
    }
    $card->rotate_right;
    push @actions, action->new( type => 2, card => $card, player_id => $player->player_id, );   # rotate
}

package startpad;
use base 'pad';
use Coro;
sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'green', pad_text => 'Activate:  Drop dog here after placing program cards and robots to start round', @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $dog = shift;
    my $player = shift;
    if( $dog->id eq 'dog' ) {

        $console->log("Activation phase!\n");

        async {
            my $timer = Coro::Event->timer( interval => 2, );
            my $step = 1;
            while( $dog->overlaps($self) and $step < 6) {
                STDERR->print("dog.  step $step.\n");
                main::simulate($step);                                                                 # iterate the simulation
                main::snapshot($step);                                                                 # generate an image
                $step++;
                cede;
                $timer->next;
            }

            # put the dog back
            $dog->x = $dog_x;
            # $dog->y = $dog->y;
            push @actions, action->new( type => 1, card => $dog, player_id => 'system', text => 'reset dog'); # move

            # discard each player's register cards
            for my $player (grep $_->isa('player'), $board->cards) {
                STDERR->print("discarding register cards for player @{[ $player->player_id ]}\n");
                for my $card ($player->cards) {
                    $player->reg_for_card($card) or next;
                    STDERR->print("discarding register cards for player @{[ $player->player_id ]}:  discarding @{[ $card->id ]} @{[ $card->front_img ]} from slot @{[ $player->reg_for_card($card) ]}\n");
                    $card->x = $discard_x;
                    $card->y = $discard_y;
                    push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'discard'); # move
                    $card->hid = 1;   # face down
                    push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
                }
            }

            # discard each player's drawn cards
            for my $player (grep $_->isa('player'), $board->cards) {
                for my $card ($player->cards) {
                    if( ! $card->hid or ( $card->x - $player->x >= 140 and $card->x - $player->x <= 357 and $card->y - $player->y >= 60 and $card->y - $player->y <= 358  ) ) {
                        STDERR->print("discarding non-register cards for player @{[ $player->player_id ]}:  discarding @{[ $card->id ]} @{[ $card->front_img ]}  \n");
                        # as above
                        $card->x = $discard_x;
                        $card->y = $discard_y;
                        push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'discard'); # move
                        $card->hid = 1;   # face down
                        push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
                    }
                }
            }

            # reset shields
            for my $player (grep $_->isa('player'), $board->cards) {
                $player->last_shield_phase = undef;
            }

            # off-board reboot spam damage
            for my $player (grep $_->isa('player'), $board->cards) {
                my $robot = $player->robot;
                # if( $robot and ( $robot->col < 0 or $robot->col > 15 or $robot->row < 0 or $robot->row > 11 ) ) # XXX
                if( $robot and ! $board->on_the_board($robot) ) {
                    next if $player->has_upgrade('upgrade-firewall');
                    STDERR->print("robot @{[ $robot->name ]} took two spam damage for running off the board and having to reboot\n");
                    $player->take_spam_damage for 1..2;
                }
            }

            $console->log("Activation phase completed.\n");

        };
    };
}

package checkpointspad;
use base 'pad';
sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'yellow', pad_text => 'Drop dog here to randomly place the checkpoints', @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift;
    my $player = shift;
    if( $card->id eq 'dog' ) {
        for my $checkpoint (@checkpoints) {
            $checkpoint->x = $board->x + 69 * 4 + 69 * int rand 12;    # move past the starting area on the left
            $checkpoint->y = $board->y + 69 * int rand 12;
            push @actions, action->new( type => 1, card => $checkpoint, player_id => 'system', text => 'place checkpoints');
        }
        $card->x = $dog_x;   # put the dog back
        push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'shuffle');
    }
}

package shufflepad;
use base 'pad';
use List::Util 'shuffle', 'min', 'max';
sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'orange', pad_text => 'Re-shuffle discard:  Drop dog to re-shuffle your discard pile', @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift or die;
    my $player = shift or die;
    if( $card->id eq 'dog' ) {
        $self->reshuffle($player);
        $card->x = $dog_x;   # put the dog back
        push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'put the dog back');
    }
}
sub reshuffle {
    my $self = shift;   # may be package as dealpad invokes us as a static method
    my $player = shift;
    STDERR->print("re-shuffling the player's discard pile for player @{[ $player->player_id ]}\n");
    my @cards = grep { $_->x >= $discard_x-30 and $_->x <= $discard_x+50 and $_->y >= $discard_y-30 and $_->y <= $discard_y+50 } $player->cards;
    for my $card (@cards) { STDERR->print("found discard card: @{[ $card->id ]} @{[ $card->front_img ]}\n"); }
    my @z_coords = List::Util::shuffle(map $_->z, @cards);
    my $x = $deck_x;
    my $y = $deck_y;
    for my $card (@cards) {
        $card->z = shift @z_coords;
        push @actions, action->new( type => 9, card => $card, player_id => 'system', text => 're-shuffle: change z');
    }
    for my $card (sort { $a->z <=> $b->z } @cards) {
        # put the card face down
        if( ! $card->hid ) {
            $card->hid = 1;
            push @actions, action->new( type => 3, card => $card, player_id => 'system', text => 're-shuffle: flip the tile back over');
        }
        # stack the pile to the right so people can see about how many cards are in there
        $card->x = $x += 2;
        $card->y = $y;
        push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 're-shuffle');
    }
}

package energypad;
use base 'pad';
sub new { my $package = shift; my $self = $package->SUPER::new( id => 'energypad', pad_color => 'lightgrey', pad_text => 'Energy bank', card_width => $energy_max_x - $energy_x, card_height => $energy_max_y - $energy_y, @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift or die;
    my $player = shift or die;
    if( grep $card->id eq $_->id, @energy ) {
        $console->log("@{[ $player->player_id ]} dropped an energy cube on the energy bank.\n");
        $player->remove_energy($card);
        # XXX unhide energy cube which we also don't have a way to do
    }
}
sub deposit_energy {
    my $self = shift;
    my $energy = shift or die;
    # XXX unhide energy
    $energy->x = int(rand $self->card_width) + $self->x;
    $energy->y = int(rand $self->card_height) + $self->y;
    push @actions, action->new( type => 1, card => $energy, player_id => 'system', text => 'energy to energybank'); # move
}

package dealpad;
use base 'pad';
sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'purple', pad_text => 'Drop dog to deal up to nine new cards after discarding the remainder of your last hand', @_ ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift or die;
    my $player = shift or die;
    if( $card->id eq 'dog' ) {
        STDERR->print("dealing up to nine cards for player @{[ $player->player_id ]}\n");
        my $find_cards = sub { sort { $b->z <=> $a->z } grep { $_->x >= $deck_x-30 and $_->x <= $deck_x+50 and $_->y >= $deck_y-30 and $_->y <= $deck_y+50 } $player->cards; };
        my @cards = $find_cards->();
        for my $card (@cards) { STDERR->print("found draw card: @{[ $card->id ]} @{[ $card->front_img ]}\n"); }
        my $to_draw = 9;
        my $x_step = 42;
        if( $player->has_upgrade('upgrade-memorystickc') ) {  # sic
            $to_draw = 10;
            $x_step = 38;
        }
        my @drawn;
        while($to_draw--) {
            if( ! @cards ) {
                STDERR->print("during draw, trying to reshuffle so we could continue to draw\n");
                shufflepad->reshuffle($player);
                @cards = $find_cards->(); 
                if( ! @cards ) {
                    STDERR->print("during draw, tried to reshuffle so we could continue to draw, but there are still no cards in the draw pile; giving up\n");
                    return;
                }
            }
            push @drawn, scalar shift @cards;
        }
        @drawn = sort { $a->z <=> $b->z } @drawn;    # when we drew, we sorted by the highest z first.  now, to make a nice staggered pile leaning right, we put the lowest z cards  first.
        for my $card (@drawn) { STDERR->print("drew card: @{[ $card->id ]} @{[ $card->front_img ]}\n"); }
        my $target_x = 147;        # XXX delt cards should have an x, y coordinate variable set
        for my $card (@drawn) {
            if( $card->hid ) {
                $card->hid = 0;
                push @actions, action->new( type => 3, card => $card, player_id => 'system', text => 'dreal');
            }
            $card->x = $player->x + $target_x;
            $card->y = $player->y + 83;
            push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'deal'); # move    XXX the card package should probably just have a move() method that takes x, y and sends a system event
            $target_x += $x_step;
        }
        $card->x = $dog_x;   # put the dog back
        push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'put the dog back');
    }
}

package upgradepad;
use base 'pad';
sub new {
    my $package = shift;
    my $self = $package->SUPER::new(
        id => $package, pad_color => 'lightgrey', pad_text => 'Upgrades<br/><br/><!-- Purchase from store with energy at the start of the round. -->', card_width => 768, card_height => 320 + 20,
        @_,
    );
    $self;
}
sub dropped_on_us {
    my $self = shift;
    my $card = shift or die;
    my $player = shift or die;
    if( grep $card->id eq $_->id, @upgrades and ! grep $card->id eq $_->id, $player->upgrades ) {
        # upgrade card dropped on us that isn't already associated with the player
        # would be nice to be able to see everyone else's upgrades but board space is limited and we're trying to re-use this patch.
        $console->log("Upgrade @{[ $card->front_img ]} added to upgrade pad for player @{[ $player->player_id ]}\n");
        $card->show_only = $player->player_id;
        push @actions, action->new( type => 8, card => $card, player_id => $player->player_id, text => $card->x . ', ' . $card->y);   # hide the card for everyone else
        $player->add_upgrade($card);
    }
}

package discardpad;
use base 'pad';
sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'lightgrey', pad_text => 'Permanent discard', card_width => 300, card_height => 190, @_, ); $self; }
sub dropped_on_us {
    my $self = shift;
    my $card = shift or die;
    my $player = shift or die;
    if( grep $card->id eq $_->id, $player->cards ) {
        # remove card from $player->cards 
        # XXX unhide which we don't have a way to do
        $console->log("@{[ $player->name ]} discarded the @{[ $card->front_img ]} card in to the permanent discard.\n");
        $player->remove_card($card);
    }
}

package console;
use base 'pad';
sub new { my $package = shift; my $self = $package->SUPER::new( id => $package, pad_color => 'lightgrey', pad_text => '', card_width => 768, card_height => 320 + 20, @_, ); $self; }
sub log {
    # sends the text to the board console pad, and sends it to STDERR
    my $self = shift;
    my $text = join '', @_;
    push @actions, action->new( type => 10, player_id => 'log', card => $self, text => $text, );   # console log text
    $text .= "\n" unless $text =~ m/\n$/;
    STDERR->print($text);
    return $text;
}
sub html {
    my $self = shift;
    return qq(
        <div id="@{[ $self->id ]}" style="position: absolute; width: @{[ $self->card_width ]}px; height: @{[ $self->card_height ]}px; left: @{[ $self->x ]}px; top: @{[ $self->y ]}px; z-index: 0; background-color: @{[ $self->pad_color ]}; overflow: scroll;">
            @{[ $self->pad_text ]}
        </div>
    );
}


#        .__                             
# ______ |  | _____  ___.__. ___________ 
# \____ \|  | \__  \<   |  |/ __ \_  __ \
# |  |_> >  |__/ __ \\___  \  ___/|  | \/
# |   __/|____(____  / ____|\___  >__|   
# |__|             \/\/         \/       

package player;
use base 'card';   # player physicalish manifests as a tray which is a kind of card

sub new {
    my $package = shift;
    my %args = @_;
    my $player_id = $args{player_id} or die;

    my $self = $package->SUPER::new(
        x => $tray_x, y => $tray_y, 
        id => 'tray_' . $player_id, 
        back_img => '/jpg/mat2.png', front_img => '/jpg/mat2.png', 
        stationary => 1, z => -1, only_show => $player_id,
        cards => [],       # movement cards, which is the player's primary deck they build from
        energy => [],      # energy cubes which are cards that can be picked up when stepped on
        upgrades => [],    # upgrade cards
        checkpoint => 0,   # last checkpoint done
        @_,
    );
    $board->add($self);

    # cards
    $self->{cards} = do {
        my @cards;
        my $card = sub {
            my $fn = shift; 
            my $card = card->new( front_img => $fn, back_img => '/jpg/cardbacksilver.png', show_only => $player_id, hid => 1, );  # scanned a likely nicer back image should maybe use XXX
            push @cards, $card;
        };
        $card->('/jpg/card-again.png') for 1..2;   # the edition I have has a per-player deck of one powerup, one move back, one u-turn, three left-turn, two again, three right turn, one move 3, three move 2, and five move one
        $card->('/jpg/card-back.png') for 1..1;
        $card->('/jpg/card-left.png') for 1..3;
        $card->('/jpg/card-right.png') for 1..3;
        $card->('/jpg/card-move3.png') for 1..1;
        $card->('/jpg/card-powerup.png') for 1..1;
        $card->('/jpg/card-uturn.png') for 1..1;
        $card->('/jpg/card_move1.png') for 1..5;
        $card->('/jpg/card_move2.png') for 1..3;
        # XXX this code probably needs to be moved where it can be invoked by itself, and with a discard pile, it's a bit more complicated too
        my $x = $deck_x;
        my $y = $deck_y;
        for my $card (List::Util::shuffle(@cards)) {
            # put the card face down
            if( ! $card->hid ) {
                $card->hid = 1;
                push @actions, action->new( type => 3, card => $card, player_id => 'system', text => 'shuffle: flip the tile back over');
            }
            # stack the pile to the right so people can see about how many cards are in there
            $card->x = $x += 2;
            $card->y = $y;
            $board->add($card); 
            push @actions, action->new( type => 1, card => $card, player_id => 'system', text => 'shuffle');
        }
        \@cards;
    };

    $self->energy_last_x = $self->x + 156;
    $self->add_energy for 1..5;

    return $self;
}
sub player_id :lvalue { $_[0]->{player_id} }
sub board { $board }     # test hook
sub password :lvalue { $_[0]->{password} }
sub cards { @{ $_[0]->{cards} } }
sub add_card { my $self = shift; my $card = shift or die; push @{ $self->{cards} }, $card; }
sub remove_card {
    # for "permanent discard"
    my $self = shift;
    my $card = shift or die;
    $self->{cards} = [ grep $card->id ne $_->id, $self->{cards}->@* ];
}
sub upgrades { @{ $_[0]->{upgrades} } }
sub has_upgrade {
    my $player = shift;
    my $upgrade = shift;
    return scalar grep $_->front_img =~ m/$upgrade/, $player->upgrades;
}
sub add_upgrade { my $self = shift; my $upgrade = shift or die; push @{ $self->{upgrades} }, $upgrade; }
sub energy { $SIG{__DIE__} = sub { Carp::confess @_; }; $_[0]->{energy}->@* }
sub add_energy {
    my $player = shift;
    my $energy = shift;
    if( ! $energy ) {
        my @available_energy = grep { $_->x >= $energy_x - 20 and $_->x <= $energy_max_x+20 and $_->y >= $energy_y-20 and $_->y <= $energy_max_y+20 } @energy;;
        if( ! @available_energy ) {
            $console->log("Tried to add energy for @{[ $player->player_id ]} but could not find any.  This is an error.\n");
            return 0;
        }
        STDERR->print("adding energy from the energy bank for @{[ $player->player_id ]}\n");
        $energy = $available_energy[0];
    } else {
        STDERR->print("adding energy handed to us for @{[ $player->player_id ]}\n");
    }
    $energy->show_only = $player->player_id;
    push @actions, action->new( type => 8, card => $energy, player_id => 'system', text => 'mark energy private');   # hide the energy for everyone else
    $energy->x = $player->energy_last_x;
    $energy->y = $player->y + 210;
    $player->energy_last_x += 35;
    push $player->{energy}->@*, $energy;
    push @actions, action->new( type => 1, card => $energy, player_id => 'system', show_only => $player->player_id, text => 'gain energy');  # move
}
sub spend_energy {
    my $player = shift;
    my $num = shift;
    if( $num > $player->energy ) {
        STDERR->print("player @{[ $player->player_id ]} unable to spend $num energy\n");
        return;
    }
    for( 1 .. $num ) {
        my $energy = shift $player->{energy}->@* or die;
        my $energybank = $board->card_by_id('energypad') or die;
        # XXX unhide the energy
        $energybank->deposit_energy($energy);
    }
    return 1;  # success
}
sub remove_energy {
    # energy was already moved to the energy bank, just need to de-allocate it from the player
    my $player = shift;
    my $energy = shift;
    $player->{energy} = [ grep $_->id ne $energy->id, $player->{energy}->@* ];
}
sub energy_last_x :lvalue { $_[0]->{energy_last_x} }
sub robot :lvalue { $_[0]->{robot} }    # player's token which for the moment is a robot
sub checkpoint :lvalue { $_[0]->{checkpoint} }
sub reg_offsets {
    return (
        undef,
        [16, 314],
        [168, 314],
        [327, 314],
        [482, 314],
        [636, 314],
    );
}
sub reg_for_card {
    my $self = shift;
    my $card = shift;
    my $card_x = $card->x - $self->x;
    my $card_y = $card->y - $self->y;
    #card card072 dropped on player's mat at 16 +- 20, 314 +- 30
    #card card072 dropped on player's mat at 168, 319 r2
    #card card072 dropped on player's mat at 327, 315 r3
    #card card072 dropped on player's mat at 482, 316 r4
    #card card072 dropped on player's mat at 636, 314 r5
    my @reg_offsets = $self->reg_offsets or die;           # top left corners of the five pads relative to the player mat's top left's x, y
    my $y_offset = $reg_offsets[1]->[1] or die;            # y coordinate should be the same for all five
    return unless $card_y >= $y_offset-30 and $card_y <= $y_offset+30;
    return unless grep $card->id eq $_->id, $self->cards;    # only recognize our own movement cards XXXXXXXXXXX testing
    my $reg;
    # places for cards are about 126 pixels apart... allow themm to be 65 pixels off one way or the other
    for my $i (1..5) {
        if( $card_x >= $reg_offsets[$i]->[0]-20 and $card_x <= $reg_offsets[$i]->[0]+45 ) {
            $reg = $i;
        }
    }
    return $reg;
}
sub registers {
    my $player = shift;
    my @registers;
    for my $card ($player->cards) {
        my $reg = $player->reg_for_card($card);
        # possible that more than one card will be in a register, but the player object is at least trying to eject the previous card in a register slot when a new one is dropped there
        $registers[$reg] = $card if defined $reg;
    }
    return @registers;    # the 0th slot will be empty
}
sub dropped_on_us {
    my $self = shift;
    my $card = shift;
    my $player = shift;
    return unless $player->player_id eq $self->player_id;    # all player mats are in the same physical space on the board, but moving something on to your mat should only affect your mat
    if( $card->hid ) {
        $card->hid = 0;
        push @actions, action->new( type => 3, card => $card, player_id => $player->player_id, );  # flip
    }
    STDERR->print("card @{[ $card->id ]} dropped on player's mat at @{[ $card->x - $self->x ]}, @{[ $card->y - $self->y ]}\n");

    my $reg = $self->reg_for_card($card);

    if( $reg ) {
        STDERR->print("card " . $card->id . " dropped on player " . $player->player_id . " mat... aligned with register $reg\n");
        for my $existing_card ($board->cards) {
            my $existing_reg = $self->reg_for_card($existing_card);
            if( defined $existing_reg and $existing_reg == $reg and $existing_card->id ne $card->id ) {
                # eject an existing card out of the register so we can place this one
                $existing_card->x = $self->x + 120;  $existing_card->y = $self->y + 46;
                push @actions, action->new( type => 1, card => $existing_card, player_id => 'system', text => 'eject from register');
                STDERR->print("card " . $card->id . " dropped on player " . $player->player_id . " mat in register $reg ejects another card, @{[ $existing_card->id ]}\n");
            }
        }
    } elsif( grep $_->id eq $card->id, @spam_cards, @energy, @special_move, ) {
        STDERR->print("spam, energy, or special_move card " . $card->id . " dropped on player " . $player->player_id . " mat\n");
        $card->show_only = $player->player_id;
        push @actions, action->new( type => 8, card => $card , player_id => 'system', text => 'mark dropped card private');   # hide the card for everyone else
        # add it to the correct deck
        if( grep $_->id eq $card->id, @spam_cards, @special_move, ) {
            push $player->{cards}->@*, $card;
            $console->log("@{[ $player->player_id ]} took a special move card from the store.\n") if grep $_->id eq $card->id, @special_move;
        } elsif( grep $_->id eq $card->id, @energy ) {
            push $player->{energy}->@*, $card;
            $console->log("@{[ $player->player_id ]} took energy from the energy bank.\n");
        }
        # edge case is if they decide they have to draw a spam for some reason and drop it directly in to a register, that won't work
    } else {
        STDERR->print("card " . $card->id . " dropped on player " . $player->player_id . " mat... did not align with a register and not spam/energy/special move\n");
    }

    # push @actions, action->new( type => 2, card => $card, player_id => $player->player_id, );   # rotate... wait, why was this here?
}
sub take_spam_damage {
    my $player = shift;
    # find a spam card to add to the player's discard pile.
    # doesn't have to be on top of the discard for any discernable reason.  that gets nicely reshuffled later.
    (my $spam) = sort { $b->z <=> $a->z } grep { $_->x >= $spam_x and $_->x <= $spam_last_x and $_->y == $spam_y } @spam_cards;
    $spam or do {
        $console->log("Couldn't find a free spam card for @{[ $player->name ]} during take_spam_damage.  This is an error.\n");
        return;
    };
    # STDERR->print("XXX spam card = @{[ $spam->id ]}\n");
    $spam->x = $discard_x;  #  discard pile doesn't stagger to show how many cards are on it nor is it labeled as a discard... kinda need a help screenshot with notes on it
    $spam->y = $discard_y;
    push @actions, action->new( type => 1, card => $spam, player_id => 'system', text => 'move spam to discard'); # move
    $spam->hid = 1;    # face down
    push @actions, action->new( type => 3, card => $spam, player_id => 'system', );  # flip
    $spam->show_only = $player->player_id;
    push @actions, action->new( type => 8, card => $spam, player_id => 'system', text => 'mark spam private');   # hide the card for everyone else
    # make the spam card part of the player's private move action card deck
    $player->add_card($spam);
}
sub damage { return scalar grep $_->isa('spam'), $_[0]->cards; }
sub last_shield_phase :lvalue { $_[0]->{last_shield_phase} }   # track the last phase shields were used on so that we don't get charged twice for the sheilds
sub presser_beam_counter :lvalue { $_[0]->{presser_beam_counter} }   # counter to fire the presser beam ever n laser shots
sub tractor_beam_counter :lvalue { $_[0]->{tractor_beam_counter} }   # counter to fire the tractor beam ever n laser shots

__END__

Todo:

* Generic card loader that takes a simple file format that lets cards be arranged in one or more piles, 
  perhaps organized by type, some delt to start with, some face down, some face up, etc.
* On card movement requests, scan the action log to see if that same card has been diddled by
  someone else in the mean time, and if so, deny the request with the appropriate message -- 
  the user probably didn't want to move the card if someone else already did.
* Perhaps allow text to be applied to cards later so we can scale them down small but have an alt tag 
  people can hoover for and get.
* Multiple independant games
* Serializing out game state
* Should increase the z-index of whatever card I most recently touched
* Money counters/cards of different sizes when loading from config
* Allow users to upload card images as well as a background to use as an optional game board
* New game creation options/game config: include turnover pad, include reveal pad, include rotate pad,
  background
* Ability to download and upload serialized games as a cheapo save/restore feature
  
Continuity
----------
* Perhaps redefine 'print' in the importer's package... but then how to refer to $request?
* Shouldn't die unless things are very seriously wrong -- that'll take out the whole app.  Log it and sally forth.
* More of a message passing backend -- at least examples of using Event to watch variables, creating queues, etc.
* DBI wrapping for multiplexing
* send_static needs to use the io 'w' event too to avoid overflowing the buffer.

..........

for i in *;do djpeg $i | pnmscale 0.5 | cjpeg > "jpg/$i";done

...

figlet -f graffiti timers
