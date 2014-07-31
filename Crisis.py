from operator import or_
from random import Random
from itertools import count, compress
from time import clock

import const
from base_player import BasePlayer

# very magic numbers that are fun to fiddle with
OCCAM = 2
SUN_TZU = 100

TIMEOUT = 9

# this is the function to mess with!
def weight_answer(remaining_ships_count, pop=None):
    part1 = (remaining_ships_count + 1) ** OCCAM
    part2 = SUN_TZU if pop is None else pop
    return part1 * part2

NORTH = 0
EAST = 1
SOUTH = 2
WEST = 3

def is_valid_coord(coord):
    """Checks if a coord represented as a 2-tuple of ints is on the board."""
    return (coord[0] >= 0 and coord[1] >= 0 and
            coord[1] < 12 and coord[0] < (6 if coord[1] < 6 else 12))

def all_coords():
    """Generates each coord on the board."""
    for y in xrange(12):
        for x in xrange(6 if y < 6 else 12):
            yield (x,y)

def coord_to_bit(coord):
    if coord[1] < 6:
        bit_pos = coord[0] + 6 * coord[1]
    else:
        bit_pos = coord[0] + 12 * coord[1] - 36
    return 1L << bit_pos

def board_to_jagged_array(board):
    result = []
    for i in range(6):
        result.append([])
        for j in range(6):
            result[-1].append(const.OCCUPIED if board & 1 else const.EMPTY)
            board >>= 1
    for i in range(6):
        result.append([])
        for j in range(12):
            result[-1].append(const.OCCUPIED if board & 1 else const.EMPTY)
            board >>= 1
    
    return result

def all_positions_of_(ship):
    if ship == const.DESTROYER:
        pattern = {(0,0),(0,1)}
    elif ship == const.CRUISER:
        pattern = {(0,0),(0,1),(0,2)}
    elif ship == const.BATTLESHIP:
        pattern = {(0,0),(0,1),(0,2),(0,3)}
    elif ship == const.HOVERCRAFT:
        pattern = {(0,0),(0,1),(1,1),(1,2),(2,0),(2,1)}
    elif ship == const.CARRIER:
        pattern = {(0,0),(0,1),(0,2),(1,1),(2,1),(3,1)}

    for x_base, y_base in all_coords():
        for direction in xrange(4):
            if direction == NORTH:
                offset = lambda (x,y):(x_base + x, y_base + y)
            elif direction == EAST:
                offset = lambda (x,y): (x_base + y, y_base - x)
            elif direction == SOUTH:
                offset = lambda (x,y): (x_base - x, y_base - y)
            elif direction == WEST:
                offset = lambda (x,y): (x_base - y, y_base + x)

            placement = map(offset, pattern)
            if all(map(is_valid_coord, placement)):
                yield reduce(or_, map(coord_to_bit, placement))

all_positions_of = {ship : list(all_positions_of_(ship)) for ship in range(10,15)}

initial_ship_list = (2**15 - 1) ^ (2**10 - 1)

def is_in_ship_list(ship_list, ship):
    return bool(ship_list & 2**ship)

def bits_of(binary_number):
    bit = 1
    while bit <= binary_number:
        yield binary_number & bit
        bit <<= 1

def popcount(binary_number):
    result = 0
    while binary_number:
        result += 1
        binary_number &= binary_number - 1
    return result

class Player(BasePlayer):
    def __init__(self):
        self._playerName = "Crisis"
        self._playerDescription = "Crisis on finite worlds."
        self.misses = 0L
        self.universes = [(0L, initial_ship_list)]
        self.empty_cells = 2**108 - 1

    def newRound(self):
        self.misses = 0L
        self.universes = [(0L, initial_ship_list)]
        self.empty_cells = 2**108 - 1

    def chooseMove(self):
        final_scores = [0]*108
        def callback(scores):
            for i, score in enumerate(scores):
                final_scores[i] += score
        empty_cells = self.empty_cells
        start = clock()
        for universe, rem_ships in self.universes:
            number_of_remaining_ships = popcount(rem_ships)
            for i in compress(count(0), bits_of(empty_cells)):
                if (1<<i) & universe != 0L:
                    final_scores[i] += weight_answer(number_of_remaining_ships, None)
                    continue
            for ship in (i for i in range(10, 15) if is_in_ship_list(rem_ships, i)):
                for placement in all_positions_of[ship]:
                    if placement & self.misses != 0L:
                        continue
                    if placement & ~empty_cells != 0L:
                        continue
                    if placement & universe != 0L:
                        continue
                    x = 0
                    y = 1
                    while y <= placement:
                        if y & placement != 0L:
                            final_scores[x] += weight_answer(number_of_remaining_ships, 1)
                        x += 1
                        y <<= 1
            if clock() - start >= TIMEOUT:
                break
        max_score = 0
        bests = []
        for coord, score in enumerate(final_scores):
            if score > max_score:
                max_score = score
                bests = [coord]
            elif score == max_score:
                bests.append(coord)
        r = Random()
        coord = r.choice(bests)
        if coord < 36:
            return (coord // 6, coord % 6)
        else:
            coord += 36
            return (coord // 12, coord % 12)

    def setOutcome(self, outcome, row, col):
        coord = coord_to_bit((col, row))
        self.empty_cells &= ~coord
        if outcome == const.MISSED:
            self.misses |= coord
            new_universes = [(a, b) for (a, b) in self.universes if coord & a == 0L]
        elif outcome == const.HIT:
            new_universes = []
            append = new_universes.append # method lookup is apparently slow
            for universe, ship_list in self.universes:
                if coord & universe == 0L:
                    for ship in (i for i in xrange(10,15) if is_in_ship_list(ship_list, i)):
                        for placement in all_positions_of[ship]:
                            if placement & coord == 0L:
                                continue
                            if placement & self.misses != 0L:
                                continue
                            if placement & universe != 0L:
                                continue
                            append((universe | placement, ship_list - 2**ship))
                else:
                    append((universe, ship_list))
        self.universes = new_universes

    def deployFleet(self):
        r = Random()
        fleet = 0L
        fleet_incomplete = True
        while fleet_incomplete:
            fleet_incomplete = False
            for ship in [const.CARRIER, const.HOVERCRAFT, const.BATTLESHIP, const.CRUISER, const.DESTROYER]:
                for z in range(100):
                    p = r.choice(all_positions_of[ship])
                    if fleet & p == 0:
                        fleet |= p
                        break
                else:
                    fleet_incomplete = True
                    break
        return board_to_jagged_array(fleet)
        
getPlayer = Player
