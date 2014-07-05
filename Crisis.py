from operator import or_

# very magic numbers that are fun to fiddle with
OCCAM = 2
SUN_TZU = 10

# this is the function to mess with!
def weight_answer(remaining_ships_count, pop=None):
    return (remaining_ships_count + 1)**OCCAM * (SUN_TZU if pop is None else pop)

MISS = False
HIT = True

DESTROYER = 0
CRUISER = 1
BATTLESHIP = 2
HOVERCRAFT = 4
AIRCRAFT_CARRIER = 5

def is_valid_ship(ship):
    return type(ship) is int and ship >= 0 and ship < 6

def is_valid_ship_list(ship_list):
    return type(ship_list) is int and ship_list >= 0 and ship_list < 64

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
    elif ship == AIRCRAFT_CARRIER:
        pattern = {(0,0),(0,1),(0,2),(1,1),(2,1),(3,1)}
    else:
        raise AssertionError

    for x_base, y_base in all_coords:
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


initial_ship_list = 2**6 - 1

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
    def __init__(self, registry, assumed_ship_positions, remaining_ships, misses):
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
        for ship in bits_of(self.__remaining_ships__):
            for placement in all_positions_of(ship):
                if placement & coord == 0L:
                    continue
                if placement & misses[0] != 0L:
                    continue
                if placement & self.__assumed_ship_positions__ != 0L:
                    continue
                Universe(self.__registry__,
                         self.__assumed_ship_positions | placement,
                         remove_from_ship_list(self.__remaining_ships__, ship),
                         misses)
        self.__remove__()

    def update(self, coord, result):
        assert(is_valid_coord(coord))
        if (result == HIT and
            coord_to_bit(coord) & self.__assumed_ship_positions__ == 0L):
            self.__spawn__(coord_to_bit(coord))
        elif result == MISS:
            self.__remove__()

    def query(self, empty_cells, callback):
        assert(type(empty_cells) is long)
        number_of_remaining_ships = popcount(self.__remaining_ships__)
        result = [0] * 108
        for i, x in enumerate(bits_of(empty_cells)):
            if x == 0:
                continue
            if x & self.__assumed_ship_positions__ != 0L:
                result[i] = None
                continue
        for ship in bits_of(self.__remaining_ships__):
            for placement in all_positions_of(ship):
                if placement & empty_cells == placement:
                    x = 0
                    y = 1
                    while y <= placement:
                        if y & placement:
                            result[x] += 1
                        x += 1
                        y <<= 1
        callback(map(lambda n: weight_answer(number_of_remaining_ships, n),
                     result))

    def __remove__(self):
        del self.__registry__[id(self)]

class Player():
    def newRound(self):
        self.misses = [0L]
        self.registry = {}
        self.empty_cells = 2*108 - 1
        Universe(registry, 0L, initial_ship_list, self.misses)
    def chooseMove(self):
        final_scores = [0]*108
        def callback(scores):
            for i, score in enumerate(scores):
                final_scores[i] += score

        #TODO: parallelize
        for universe in self.registry.itervalues():
            universe.query(self.empty_cells, callback)

        max_score = 0
        bests = []
        for coord, score in enumerate(final_scores):
            if score > max_score:
                max_score = score
                bests = [coord]
            elif score == max_score:
                bests.append(coord)

        #TODO: choose randomly
        coord = bests[0]
        if coord < 36:
            return (coord % 6, coord // 6)
        else:
            coord += 36
            return (coord % 12, coord // 12)

    def setOutcome(self, outcome):
        pass
