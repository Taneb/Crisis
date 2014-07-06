from operator import or_
from random import Random

# import const
# from base_player import BasePlayer

UNKOWN = 0
EMPTY = 1
OCCUPIED = 2
MISSED = 3
HIT = 4

CARRIER = 10
HOVERCRAFT = 11
BATTLESHIP = 12
CRUISER = 13
DESTROYER = 14

class BasePlayer:
    # Initialise the boards: player to empty, opponent to unknown
    def __init__(self):
        self._playerName = "Unknown"
        self._playerDescription = "None"
    
    def getName(self):
        return self._playerName


    def getDescription(self):
        return self._playerDescription

       # Distribute the fleet onto your board
    def deployFleet(self):
        """
        Decide where you want your fleet to be deployed, then return your board. 
        """
        pass


    # Decide what move to make based on current state of opponent's board and print it out
    def chooseMove(self):
        """
        Decide what move to make based on current state of opponent's board and return it 
        """

        pass


    def setOutcome(self, entry, i1, i2):
        """
        Read the outcome of the shot from the keyboard
        expected value is const.HIT for hit and const.MISSED for missed
        """
        pass


    def getOpponentMove(self, i1,i2):
        """ You might like to keep track of where your opponent has missed, but here we just acknowledge it
        """
        pass

    def newRound(self):

        pass

    def newPlayer(self, name = None):

        pass


# very magic numbers that are fun to fiddle with
OCCAM = 2
SUN_TZU = 10000

# this is the function to mess with!
def weight_answer(remaining_ships_count, pop=None):
    part1 = (remaining_ships_count + 1) ** OCCAM
    part2 = SUN_TZU if pop is None else pop
    return part1 * part2

def is_valid_ship(ship):
    return type(ship) is int and ship >= 10 and ship < 15

def is_valid_ship_list(ship_list):
    return True
    return (type(ship_list) is int and ship_list >= 0 and 
            ship_list <= 31744  and ship_list % 1024 == 0)

NORTH = 0
EAST = 1
SOUTH = 2
WEST = 3

def is_valid_direction(direction):
    return type(direction) is int and direction >= 0 and direction < 4

def is_valid_coord(coord):
    if type(coord) is not tuple:
        return False
    if len(coord) != 2:
        return False
    if type(coord[0]) is not int or type(coord[1]) is not int:
        return False
    return (coord[0] >= 0 and coord[1] >= 0 and
            coord[1] < 12 and coord[0] < (6 if coord[1] < 6 else 12))

def all_coords():
    for y in xrange(12):
        for x in xrange(6 if y < 6 else 12):
            assert(is_valid_coord((x,y)))
            yield (x,y)

def coord_to_bit(coord):
    assert(is_valid_coord(coord))
    if coord[1] < 6:
        bit_pos = coord[0] + 6 * coord[1]
    else:
        bit_pos = coord[0] + 12 * coord[1] - 36
    return 2L ** bit_pos

def board_to_jagged_array(board):
    assert type(board) is long
    assert board >= 0
    assert board < 2**108
    
    result = []
    for i in range(6):
        result.append([])
        for j in range(6):
            result[-1].append(OCCUPIED if board & 1 else EMPTY)
            board >>= 1
    for i in range(6):
        result.append([])
        for j in range(12):
            result[-1].append(OCCUPIED if board & 1 else EMPTY)
            board >>= 1
    
    return result

def all_positions_of(ship):
    assert(is_valid_ship(ship))
    if ship == DESTROYER:
        pattern = {(0,0),(0,1)}
    elif ship == CRUISER:
        pattern = {(0,0),(0,1),(0,2)}
    elif ship == BATTLESHIP:
        pattern = {(0,0),(0,1),(0,2),(0,3)}
    elif ship == HOVERCRAFT:
        pattern = {(0,0),(0,1),(1,1),(1,2),(2,0),(2,1)}
    elif ship == CARRIER:
        pattern = {(0,0),(0,1),(0,2),(1,1),(2,1),(3,1)}
    else:
        raise AssertionError

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
            else:
                raise AssertionError

            placement = map(offset, pattern)
            if all(map(is_valid_coord, placement)):
                yield reduce(or_, map(coord_to_bit, placement))


initial_ship_list = (2**15 - 1) ^ (2**9 - 1)
assert is_valid_ship_list(initial_ship_list)

def is_in_ship_list(ship_list, ship):
    assert(is_valid_ship_list(ship_list))
    assert(is_valid_ship(ship))
    return bool(ship_list & 2**ship)

def remove_from_ship_list(ship_list, ship):
    assert(is_valid_ship_list(ship_list))
    assert(is_valid_ship(ship))
    assert(is_in_ship_list(ship_list, ship))
    return ship_list - 2 ** ship

def bits_of(binary_number):
    assert type(binary_number) is int or type(binary_number) is long
    assert binary_number >= 0
    bit = 1
    while bit <= binary_number:
        yield binary_number & bit
        bit <<= 1

def popcount(binary_number):
    assert type(binary_number) is int or type(binary_number) is long
    assert binary_number >= 0
    result = 0
    while binary_number:
        result += 1
        binary_number &= binary_number - 1
    return result

class Universe():
    def __init__(self, registry, assumed_ship_positions, 
                 remaining_ships, misses):
        assert(type(registry) is dict)
        self.__registry__ = registry
        registry[id(self)] = self
        assert(type(assumed_ship_positions) is long)
        self.__assumed_ship_positions__ = assumed_ship_positions
        assert(is_valid_ship_list(remaining_ships))
        self.__remaining_ships__ = remaining_ships
        # TODO: figure a nice way to have mutable long ints
        # one-length lists are nasty :(
        assert(type(misses) is list and len(misses) == 1)
        assert(type(misses[0]) is long)
        self.__misses__ = misses

    def __spawn__(self, coord):
        for ship in [i for i in range(10, 15) if is_in_ship_list(
                self.__remaining_ships__, i)]:
            for placement in all_positions_of(ship):
                if placement & coord == 0L:
                    continue
                if placement & self.__misses__[0] != 0L:
                    continue
                if placement & self.__assumed_ship_positions__ != 0L:
                    continue
                Universe(self.__registry__,
                         self.__assumed_ship_positions__ | placement,
                         remove_from_ship_list(self.__remaining_ships__, ship),
                         self.__misses__)
        self.__remove__()

    def update(self, coord, result):
        assert(is_valid_coord(coord))
        if (result == HIT and
            coord_to_bit(coord) & self.__assumed_ship_positions__ == 0L):
            self.__spawn__(coord_to_bit(coord))
        elif (result == MISSED and
              coord_to_bit(coord) & self.__assumed_ship_positions__ != 0L):
            self.__remove__()

    def query(self, empty_cells, callback):
        assert(type(empty_cells) is long)
        number_of_remaining_ships = popcount(self.__remaining_ships__)
        result = [0] * 108

        # set all empty coords that we are assuming are hits to None
        for i, x in enumerate(bits_of(empty_cells)):
            if not bool(x):
                continue
            if (1<<i) & self.__assumed_ship_positions__ != 0L:
                result[i] = None
                continue


        for ship in [i for i in range(10, 15) if is_in_ship_list(
                self.__remaining_ships__, i)]:
            for placement in all_positions_of(ship):
                if placement & self.__misses__[0] != 0L:
                    continue
                if placement & ~empty_cells:
                    continue
                x = 0
                y = 1
                while y <= placement:
                    if y & placement != 0L and result[x] is not None:
                        result[x] += 1
                    x += 1
                    y <<= 1
        callback(map(lambda n: weight_answer(number_of_remaining_ships, n),
                     result))

    def __remove__(self):
        del self.__registry__[id(self)]

class Player(BasePlayer):
    def __init__(self):
        self._playerName = "Crisis"
        self._playerDescription = "Crisis on finite worlds."
        self.misses = [0L]
        self.registry = {}
        self.empty_cells = 2**108 - 1

    def newRound(self):
        self.misses = [0L]
        self.registry = {}
        self.empty_cells = 2**108 - 1
        Universe(self.registry, 0L, initial_ship_list, self.misses)

    def chooseMove(self):
        print len(self.registry)
        final_scores = [0]*108
        def callback(scores):
            for i, score in enumerate(scores):
                final_scores[i] += score

        #TODO: parallelize
        for universe in list(self.registry.itervalues()):
            universe.query(self.empty_cells, callback)

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
        self.empty_cells &= ~coord_to_bit((col,row))
        if outcome == MISSED:
            self.misses[0] |= coord_to_bit((col, row))
        for universe in list(self.registry.itervalues()):
            universe.update((col, row), outcome)

    def deployFleet(self):
        r = Random()
        fleet = 0L
        while True:
            a = r.choice(list(all_positions_of(CARRIER)))
            if fleet & a == 0:
                fleet |= a
            else:
                continue
            for z in range(100):
                h = r.choice(list(all_positions_of(HOVERCRAFT)))
                if fleet & h == 0:
                    fleet |= h
                    break
            else:
                continue
            for z in range(100):
                b = r.choice(list(all_positions_of(BATTLESHIP)))
                if fleet & b == 0:
                    fleet |= b
                    break
            else:
                continue
            for z in range(100):
                c = r.choice(list(all_positions_of(CRUISER)))
                if fleet & c == 0:
                    fleet |= c
                    break
            else:
                continue
            for z in range(100):
                d = r.choice(list(all_positions_of(DESTROYER)))
                if fleet & d == 0:
                    fleet |= c
                    break
            else:
                continue
            break
        return board_to_jagged_array(fleet)
        
getPlayer = lambda: Player()
