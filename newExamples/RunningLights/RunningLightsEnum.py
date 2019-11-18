from enum import Enum

class RunningLightsEnum(object):
    
    def __init__(self):
        self._state = 128
    
    class LightNumber(Enum):
        L0 = 0
        L1 = 1
        L2 = 2
        L3 = 3
        L4 = 4
        L5 = 5
        L6 = 6
        L7 = 7
    
    _table = [ {
        (False,False): (0, (False,False,LightNumber.L0)),
            (False,True): (1, (False,False,LightNumber.L0)),
            (True,False): (2, (False,False,LightNumber.L0)),
            (True,True): (3, (False,False,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (0, (False,False,LightNumber.L0)),
            (False,True): (1, (False,False,LightNumber.L0)),
            (True,False): (2, (False,False,LightNumber.L0)),
            (True,True): (3, (False,False,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (8, (False,True,LightNumber.L0)),
            (False,True): (9, (False,True,LightNumber.L0)),
            (True,False): (10, (False,True,LightNumber.L0)),
            (True,True): (11, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (8, (False,True,LightNumber.L0)),
            (False,True): (9, (False,True,LightNumber.L0)),
            (True,False): (10, (False,True,LightNumber.L0)),
            (True,True): (11, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (8, (False,True,LightNumber.L0)),
            (False,True): (9, (False,True,LightNumber.L0)),
            (True,False): (10, (False,True,LightNumber.L0)),
            (True,True): (11, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (8, (False,True,LightNumber.L0)),
            (False,True): (9, (False,True,LightNumber.L0)),
            (True,False): (10, (False,True,LightNumber.L0)),
            (True,True): (11, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (12, (False,True,LightNumber.L1)),
            (False,True): (13, (False,True,LightNumber.L1)),
            (True,False): (14, (False,True,LightNumber.L1)),
            (True,True): (15, (False,True,LightNumber.L1)),
        },
        {
        (False,False): (12, (False,True,LightNumber.L1)),
            (False,True): (13, (False,True,LightNumber.L1)),
            (True,False): (14, (False,True,LightNumber.L1)),
            (True,True): (15, (False,True,LightNumber.L1)),
        },
        {
        (False,False): (12, (False,True,LightNumber.L1)),
            (False,True): (13, (False,True,LightNumber.L1)),
            (True,False): (14, (False,True,LightNumber.L1)),
            (True,True): (15, (False,True,LightNumber.L1)),
        },
        {
        (False,False): (12, (False,True,LightNumber.L1)),
            (False,True): (13, (False,True,LightNumber.L1)),
            (True,False): (14, (False,True,LightNumber.L1)),
            (True,True): (15, (False,True,LightNumber.L1)),
        },
        {
        (False,False): (16, (False,True,LightNumber.L2)),
            (False,True): (17, (False,True,LightNumber.L2)),
            (True,False): (18, (False,True,LightNumber.L2)),
            (True,True): (19, (False,True,LightNumber.L2)),
        },
        {
        (False,False): (16, (False,True,LightNumber.L2)),
            (False,True): (17, (False,True,LightNumber.L2)),
            (True,False): (18, (False,True,LightNumber.L2)),
            (True,True): (19, (False,True,LightNumber.L2)),
        },
        {
        (False,False): (20, (True,True,LightNumber.L2)),
            (False,True): (21, (True,True,LightNumber.L2)),
            (True,False): (22, (True,True,LightNumber.L2)),
            (True,True): (23, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (20, (True,True,LightNumber.L2)),
            (False,True): (21, (True,True,LightNumber.L2)),
            (True,False): (22, (True,True,LightNumber.L2)),
            (True,True): (23, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (24, (False,True,LightNumber.L3)),
            (False,True): (25, (False,True,LightNumber.L3)),
            (True,False): (26, (False,True,LightNumber.L3)),
            (True,True): (27, (False,True,LightNumber.L3)),
        },
        {
        (False,False): (24, (False,True,LightNumber.L3)),
            (False,True): (25, (False,True,LightNumber.L3)),
            (True,False): (26, (False,True,LightNumber.L3)),
            (True,True): (27, (False,True,LightNumber.L3)),
        },
        {
        (False,False): (28, (True,True,LightNumber.L3)),
            (False,True): (29, (True,True,LightNumber.L3)),
            (True,False): (30, (True,True,LightNumber.L3)),
            (True,True): (31, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (28, (True,True,LightNumber.L3)),
            (False,True): (29, (True,True,LightNumber.L3)),
            (True,False): (30, (True,True,LightNumber.L3)),
            (True,True): (31, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (28, (True,True,LightNumber.L3)),
            (False,True): (29, (True,True,LightNumber.L3)),
            (True,False): (30, (True,True,LightNumber.L3)),
            (True,True): (31, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (28, (True,True,LightNumber.L3)),
            (False,True): (29, (True,True,LightNumber.L3)),
            (True,False): (30, (True,True,LightNumber.L3)),
            (True,True): (31, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (28, (True,True,LightNumber.L3)),
            (False,True): (29, (True,True,LightNumber.L3)),
            (True,False): (30, (True,True,LightNumber.L3)),
            (True,True): (31, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (28, (True,True,LightNumber.L3)),
            (False,True): (29, (True,True,LightNumber.L3)),
            (True,False): (30, (True,True,LightNumber.L3)),
            (True,True): (31, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (32, (False,True,LightNumber.L4)),
            (False,True): (33, (False,True,LightNumber.L4)),
            (True,False): (34, (False,True,LightNumber.L4)),
            (True,True): (35, (False,True,LightNumber.L4)),
        },
        {
        (False,False): (32, (False,True,LightNumber.L4)),
            (False,True): (33, (False,True,LightNumber.L4)),
            (True,False): (34, (False,True,LightNumber.L4)),
            (True,True): (35, (False,True,LightNumber.L4)),
        },
        {
        (False,False): (36, (True,True,LightNumber.L4)),
            (False,True): (37, (True,True,LightNumber.L4)),
            (True,False): (38, (True,True,LightNumber.L4)),
            (True,True): (39, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (36, (True,True,LightNumber.L4)),
            (False,True): (37, (True,True,LightNumber.L4)),
            (True,False): (38, (True,True,LightNumber.L4)),
            (True,True): (39, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (36, (True,True,LightNumber.L4)),
            (False,True): (37, (True,True,LightNumber.L4)),
            (True,False): (38, (True,True,LightNumber.L4)),
            (True,True): (39, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (36, (True,True,LightNumber.L4)),
            (False,True): (37, (True,True,LightNumber.L4)),
            (True,False): (38, (True,True,LightNumber.L4)),
            (True,True): (39, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (36, (True,True,LightNumber.L4)),
            (False,True): (37, (True,True,LightNumber.L4)),
            (True,False): (38, (True,True,LightNumber.L4)),
            (True,True): (39, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (36, (True,True,LightNumber.L4)),
            (False,True): (37, (True,True,LightNumber.L4)),
            (True,False): (38, (True,True,LightNumber.L4)),
            (True,True): (39, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (40, (False,True,LightNumber.L5)),
            (False,True): (41, (False,True,LightNumber.L5)),
            (True,False): (42, (False,True,LightNumber.L5)),
            (True,True): (43, (False,True,LightNumber.L5)),
        },
        {
        (False,False): (40, (False,True,LightNumber.L5)),
            (False,True): (41, (False,True,LightNumber.L5)),
            (True,False): (42, (False,True,LightNumber.L5)),
            (True,True): (43, (False,True,LightNumber.L5)),
        },
        {
        (False,False): (44, (True,True,LightNumber.L5)),
            (False,True): (45, (True,True,LightNumber.L5)),
            (True,False): (46, (True,True,LightNumber.L5)),
            (True,True): (47, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (44, (True,True,LightNumber.L5)),
            (False,True): (45, (True,True,LightNumber.L5)),
            (True,False): (46, (True,True,LightNumber.L5)),
            (True,True): (47, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (44, (True,True,LightNumber.L5)),
            (False,True): (45, (True,True,LightNumber.L5)),
            (True,False): (46, (True,True,LightNumber.L5)),
            (True,True): (47, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (44, (True,True,LightNumber.L5)),
            (False,True): (45, (True,True,LightNumber.L5)),
            (True,False): (46, (True,True,LightNumber.L5)),
            (True,True): (47, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (44, (True,True,LightNumber.L5)),
            (False,True): (45, (True,True,LightNumber.L5)),
            (True,False): (46, (True,True,LightNumber.L5)),
            (True,True): (47, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (44, (True,True,LightNumber.L5)),
            (False,True): (45, (True,True,LightNumber.L5)),
            (True,False): (46, (True,True,LightNumber.L5)),
            (True,True): (47, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (48, (False,True,LightNumber.L6)),
            (False,True): (49, (False,True,LightNumber.L6)),
            (True,False): (50, (False,True,LightNumber.L6)),
            (True,True): (51, (False,True,LightNumber.L6)),
        },
        {
        (False,False): (48, (False,True,LightNumber.L6)),
            (False,True): (49, (False,True,LightNumber.L6)),
            (True,False): (50, (False,True,LightNumber.L6)),
            (True,True): (51, (False,True,LightNumber.L6)),
        },
        {
        (False,False): (52, (True,True,LightNumber.L6)),
            (False,True): (53, (True,True,LightNumber.L6)),
            (True,False): (54, (True,True,LightNumber.L6)),
            (True,True): (55, (True,True,LightNumber.L6)),
        },
        {
        (False,False): (52, (True,True,LightNumber.L6)),
            (False,True): (53, (True,True,LightNumber.L6)),
            (True,False): (54, (True,True,LightNumber.L6)),
            (True,True): (55, (True,True,LightNumber.L6)),
        },
        {
        (False,False): (52, (True,True,LightNumber.L6)),
            (False,True): (53, (True,True,LightNumber.L6)),
            (True,False): (54, (True,True,LightNumber.L6)),
            (True,True): (55, (True,True,LightNumber.L6)),
        },
        {
        (False,False): (52, (True,True,LightNumber.L6)),
            (False,True): (53, (True,True,LightNumber.L6)),
            (True,False): (54, (True,True,LightNumber.L6)),
            (True,True): (55, (True,True,LightNumber.L6)),
        },
        {
        (False,False): (52, (True,True,LightNumber.L6)),
            (False,True): (53, (True,True,LightNumber.L6)),
            (True,False): (54, (True,True,LightNumber.L6)),
            (True,True): (55, (True,True,LightNumber.L6)),
        },
        {
        (False,False): (52, (True,True,LightNumber.L6)),
            (False,True): (53, (True,True,LightNumber.L6)),
            (True,False): (54, (True,True,LightNumber.L6)),
            (True,True): (55, (True,True,LightNumber.L6)),
        },
        {
        (False,False): (56, (False,True,LightNumber.L7)),
            (False,True): (57, (False,True,LightNumber.L7)),
            (True,False): (58, (False,True,LightNumber.L7)),
            (True,True): (59, (False,True,LightNumber.L7)),
        },
        {
        (False,False): (56, (False,True,LightNumber.L7)),
            (False,True): (57, (False,True,LightNumber.L7)),
            (True,False): (58, (False,True,LightNumber.L7)),
            (True,True): (59, (False,True,LightNumber.L7)),
        },
        {
        (False,False): (60, (True,True,LightNumber.L7)),
            (False,True): (61, (True,True,LightNumber.L7)),
            (True,False): (62, (True,True,LightNumber.L7)),
            (True,True): (63, (True,True,LightNumber.L7)),
        },
        {
        (False,False): (60, (True,True,LightNumber.L7)),
            (False,True): (61, (True,True,LightNumber.L7)),
            (True,False): (62, (True,True,LightNumber.L7)),
            (True,True): (63, (True,True,LightNumber.L7)),
        },
        {
        (False,False): (60, (True,True,LightNumber.L7)),
            (False,True): (61, (True,True,LightNumber.L7)),
            (True,False): (62, (True,True,LightNumber.L7)),
            (True,True): (63, (True,True,LightNumber.L7)),
        },
        {
        (False,False): (60, (True,True,LightNumber.L7)),
            (False,True): (61, (True,True,LightNumber.L7)),
            (True,False): (62, (True,True,LightNumber.L7)),
            (True,True): (63, (True,True,LightNumber.L7)),
        },
        {
        (False,False): (60, (True,True,LightNumber.L7)),
            (False,True): (61, (True,True,LightNumber.L7)),
            (True,False): (62, (True,True,LightNumber.L7)),
            (True,True): (63, (True,True,LightNumber.L7)),
        },
        {
        (False,False): (60, (True,True,LightNumber.L7)),
            (False,True): (61, (True,True,LightNumber.L7)),
            (True,False): (62, (True,True,LightNumber.L7)),
            (True,True): (63, (True,True,LightNumber.L7)),
        },
        {
        (False,False): (64, (False,False,LightNumber.L7)),
            (False,True): (65, (False,False,LightNumber.L7)),
            (True,False): (66, (False,False,LightNumber.L7)),
            (True,True): (67, (False,False,LightNumber.L7)),
        },
        {
        (False,False): (64, (False,False,LightNumber.L7)),
            (False,True): (65, (False,False,LightNumber.L7)),
            (True,False): (66, (False,False,LightNumber.L7)),
            (True,True): (67, (False,False,LightNumber.L7)),
        },
        {
        (False,False): (68, (True,False,LightNumber.L6)),
            (False,True): (69, (True,False,LightNumber.L6)),
            (True,False): (70, (True,False,LightNumber.L6)),
            (True,True): (71, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (68, (True,False,LightNumber.L6)),
            (False,True): (69, (True,False,LightNumber.L6)),
            (True,False): (70, (True,False,LightNumber.L6)),
            (True,True): (71, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (68, (True,False,LightNumber.L6)),
            (False,True): (69, (True,False,LightNumber.L6)),
            (True,False): (70, (True,False,LightNumber.L6)),
            (True,True): (71, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (68, (True,False,LightNumber.L6)),
            (False,True): (69, (True,False,LightNumber.L6)),
            (True,False): (70, (True,False,LightNumber.L6)),
            (True,True): (71, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (68, (True,False,LightNumber.L6)),
            (False,True): (69, (True,False,LightNumber.L6)),
            (True,False): (70, (True,False,LightNumber.L6)),
            (True,True): (71, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (68, (True,False,LightNumber.L6)),
            (False,True): (69, (True,False,LightNumber.L6)),
            (True,False): (70, (True,False,LightNumber.L6)),
            (True,True): (71, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (64, (False,False,LightNumber.L7)),
            (False,True): (65, (False,False,LightNumber.L7)),
            (True,False): (66, (False,False,LightNumber.L7)),
            (True,True): (67, (False,False,LightNumber.L7)),
        },
        {
        (False,False): (64, (False,False,LightNumber.L7)),
            (False,True): (65, (False,False,LightNumber.L7)),
            (True,False): (66, (False,False,LightNumber.L7)),
            (True,True): (67, (False,False,LightNumber.L7)),
        },
        {
        (False,False): (72, (True,False,LightNumber.L7)),
            (False,True): (73, (True,False,LightNumber.L7)),
            (True,False): (74, (True,False,LightNumber.L7)),
            (True,True): (75, (True,False,LightNumber.L7)),
        },
        {
        (False,False): (72, (True,False,LightNumber.L7)),
            (False,True): (73, (True,False,LightNumber.L7)),
            (True,False): (74, (True,False,LightNumber.L7)),
            (True,True): (75, (True,False,LightNumber.L7)),
        },
        {
        (False,False): (76, (True,False,LightNumber.L5)),
            (False,True): (77, (True,False,LightNumber.L5)),
            (True,False): (78, (True,False,LightNumber.L5)),
            (True,True): (79, (True,False,LightNumber.L5)),
        },
        {
        (False,False): (80, (True,True,LightNumber.L5)),
            (False,True): (81, (True,True,LightNumber.L5)),
            (True,False): (82, (True,True,LightNumber.L5)),
            (True,True): (83, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (76, (True,False,LightNumber.L5)),
            (False,True): (77, (True,False,LightNumber.L5)),
            (True,False): (78, (True,False,LightNumber.L5)),
            (True,True): (79, (True,False,LightNumber.L5)),
        },
        {
        (False,False): (80, (True,True,LightNumber.L5)),
            (False,True): (81, (True,True,LightNumber.L5)),
            (True,False): (82, (True,True,LightNumber.L5)),
            (True,True): (83, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (84, (True,False,LightNumber.L6)),
            (False,True): (85, (True,False,LightNumber.L6)),
            (True,False): (86, (True,False,LightNumber.L6)),
            (True,True): (87, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (84, (True,False,LightNumber.L6)),
            (False,True): (85, (True,False,LightNumber.L6)),
            (True,False): (86, (True,False,LightNumber.L6)),
            (True,True): (87, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (84, (True,False,LightNumber.L6)),
            (False,True): (85, (True,False,LightNumber.L6)),
            (True,False): (86, (True,False,LightNumber.L6)),
            (True,True): (87, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (84, (True,False,LightNumber.L6)),
            (False,True): (85, (True,False,LightNumber.L6)),
            (True,False): (86, (True,False,LightNumber.L6)),
            (True,True): (87, (True,False,LightNumber.L6)),
        },
        {
        (False,False): (88, (True,False,LightNumber.L4)),
            (False,True): (89, (True,False,LightNumber.L4)),
            (True,False): (90, (True,False,LightNumber.L4)),
            (True,True): (91, (True,False,LightNumber.L4)),
        },
        {
        (False,False): (92, (True,True,LightNumber.L4)),
            (False,True): (93, (True,True,LightNumber.L4)),
            (True,False): (94, (True,True,LightNumber.L4)),
            (True,True): (95, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (88, (True,False,LightNumber.L4)),
            (False,True): (89, (True,False,LightNumber.L4)),
            (True,False): (90, (True,False,LightNumber.L4)),
            (True,True): (91, (True,False,LightNumber.L4)),
        },
        {
        (False,False): (92, (True,True,LightNumber.L4)),
            (False,True): (93, (True,True,LightNumber.L4)),
            (True,False): (94, (True,True,LightNumber.L4)),
            (True,True): (95, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (92, (True,True,LightNumber.L4)),
            (False,True): (93, (True,True,LightNumber.L4)),
            (True,False): (94, (True,True,LightNumber.L4)),
            (True,True): (95, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (92, (True,True,LightNumber.L4)),
            (False,True): (93, (True,True,LightNumber.L4)),
            (True,False): (94, (True,True,LightNumber.L4)),
            (True,True): (95, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (92, (True,True,LightNumber.L4)),
            (False,True): (93, (True,True,LightNumber.L4)),
            (True,False): (94, (True,True,LightNumber.L4)),
            (True,True): (95, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (92, (True,True,LightNumber.L4)),
            (False,True): (93, (True,True,LightNumber.L4)),
            (True,False): (94, (True,True,LightNumber.L4)),
            (True,True): (95, (True,True,LightNumber.L4)),
        },
        {
        (False,False): (76, (True,False,LightNumber.L5)),
            (False,True): (77, (True,False,LightNumber.L5)),
            (True,False): (78, (True,False,LightNumber.L5)),
            (True,True): (79, (True,False,LightNumber.L5)),
        },
        {
        (False,False): (80, (True,True,LightNumber.L5)),
            (False,True): (81, (True,True,LightNumber.L5)),
            (True,False): (82, (True,True,LightNumber.L5)),
            (True,True): (83, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (76, (True,False,LightNumber.L5)),
            (False,True): (77, (True,False,LightNumber.L5)),
            (True,False): (78, (True,False,LightNumber.L5)),
            (True,True): (79, (True,False,LightNumber.L5)),
        },
        {
        (False,False): (80, (True,True,LightNumber.L5)),
            (False,True): (81, (True,True,LightNumber.L5)),
            (True,False): (82, (True,True,LightNumber.L5)),
            (True,True): (83, (True,True,LightNumber.L5)),
        },
        {
        (False,False): (96, (True,False,LightNumber.L3)),
            (False,True): (97, (True,False,LightNumber.L3)),
            (True,False): (98, (True,False,LightNumber.L3)),
            (True,True): (99, (True,False,LightNumber.L3)),
        },
        {
        (False,False): (100, (True,True,LightNumber.L3)),
            (False,True): (101, (True,True,LightNumber.L3)),
            (True,False): (102, (True,True,LightNumber.L3)),
            (True,True): (103, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (96, (True,False,LightNumber.L3)),
            (False,True): (97, (True,False,LightNumber.L3)),
            (True,False): (98, (True,False,LightNumber.L3)),
            (True,True): (99, (True,False,LightNumber.L3)),
        },
        {
        (False,False): (100, (True,True,LightNumber.L3)),
            (False,True): (101, (True,True,LightNumber.L3)),
            (True,False): (102, (True,True,LightNumber.L3)),
            (True,True): (103, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (100, (True,True,LightNumber.L3)),
            (False,True): (101, (True,True,LightNumber.L3)),
            (True,False): (102, (True,True,LightNumber.L3)),
            (True,True): (103, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (100, (True,True,LightNumber.L3)),
            (False,True): (101, (True,True,LightNumber.L3)),
            (True,False): (102, (True,True,LightNumber.L3)),
            (True,True): (103, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (100, (True,True,LightNumber.L3)),
            (False,True): (101, (True,True,LightNumber.L3)),
            (True,False): (102, (True,True,LightNumber.L3)),
            (True,True): (103, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (100, (True,True,LightNumber.L3)),
            (False,True): (101, (True,True,LightNumber.L3)),
            (True,False): (102, (True,True,LightNumber.L3)),
            (True,True): (103, (True,True,LightNumber.L3)),
        },
        {
        (False,False): (104, (True,False,LightNumber.L2)),
            (False,True): (105, (True,False,LightNumber.L2)),
            (True,False): (106, (True,False,LightNumber.L2)),
            (True,True): (107, (True,False,LightNumber.L2)),
        },
        {
        (False,False): (108, (True,True,LightNumber.L2)),
            (False,True): (109, (True,True,LightNumber.L2)),
            (True,False): (110, (True,True,LightNumber.L2)),
            (True,True): (111, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (104, (True,False,LightNumber.L2)),
            (False,True): (105, (True,False,LightNumber.L2)),
            (True,False): (106, (True,False,LightNumber.L2)),
            (True,True): (107, (True,False,LightNumber.L2)),
        },
        {
        (False,False): (108, (True,True,LightNumber.L2)),
            (False,True): (109, (True,True,LightNumber.L2)),
            (True,False): (110, (True,True,LightNumber.L2)),
            (True,True): (111, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (108, (True,True,LightNumber.L2)),
            (False,True): (109, (True,True,LightNumber.L2)),
            (True,False): (110, (True,True,LightNumber.L2)),
            (True,True): (111, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (108, (True,True,LightNumber.L2)),
            (False,True): (109, (True,True,LightNumber.L2)),
            (True,False): (110, (True,True,LightNumber.L2)),
            (True,True): (111, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (108, (True,True,LightNumber.L2)),
            (False,True): (109, (True,True,LightNumber.L2)),
            (True,False): (110, (True,True,LightNumber.L2)),
            (True,True): (111, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (108, (True,True,LightNumber.L2)),
            (False,True): (109, (True,True,LightNumber.L2)),
            (True,False): (110, (True,True,LightNumber.L2)),
            (True,True): (111, (True,True,LightNumber.L2)),
        },
        {
        (False,False): (112, (True,False,LightNumber.L1)),
            (False,True): (113, (True,False,LightNumber.L1)),
            (True,False): (114, (True,False,LightNumber.L1)),
            (True,True): (115, (True,False,LightNumber.L1)),
        },
        {
        (False,False): (116, (True,True,LightNumber.L1)),
            (False,True): (117, (True,True,LightNumber.L1)),
            (True,False): (118, (True,True,LightNumber.L1)),
            (True,True): (119, (True,True,LightNumber.L1)),
        },
        {
        (False,False): (112, (True,False,LightNumber.L1)),
            (False,True): (113, (True,False,LightNumber.L1)),
            (True,False): (114, (True,False,LightNumber.L1)),
            (True,True): (115, (True,False,LightNumber.L1)),
        },
        {
        (False,False): (116, (True,True,LightNumber.L1)),
            (False,True): (117, (True,True,LightNumber.L1)),
            (True,False): (118, (True,True,LightNumber.L1)),
            (True,True): (119, (True,True,LightNumber.L1)),
        },
        {
        (False,False): (116, (True,True,LightNumber.L1)),
            (False,True): (117, (True,True,LightNumber.L1)),
            (True,False): (118, (True,True,LightNumber.L1)),
            (True,True): (119, (True,True,LightNumber.L1)),
        },
        {
        (False,False): (116, (True,True,LightNumber.L1)),
            (False,True): (117, (True,True,LightNumber.L1)),
            (True,False): (118, (True,True,LightNumber.L1)),
            (True,True): (119, (True,True,LightNumber.L1)),
        },
        {
        (False,False): (116, (True,True,LightNumber.L1)),
            (False,True): (117, (True,True,LightNumber.L1)),
            (True,False): (118, (True,True,LightNumber.L1)),
            (True,True): (119, (True,True,LightNumber.L1)),
        },
        {
        (False,False): (116, (True,True,LightNumber.L1)),
            (False,True): (117, (True,True,LightNumber.L1)),
            (True,False): (118, (True,True,LightNumber.L1)),
            (True,True): (119, (True,True,LightNumber.L1)),
        },
        {
        (False,False): (120, (True,False,LightNumber.L0)),
            (False,True): (121, (True,False,LightNumber.L0)),
            (True,False): (122, (True,False,LightNumber.L0)),
            (True,True): (123, (True,False,LightNumber.L0)),
        },
        {
        (False,False): (124, (True,True,LightNumber.L0)),
            (False,True): (125, (True,True,LightNumber.L0)),
            (True,False): (126, (True,True,LightNumber.L0)),
            (True,True): (127, (True,True,LightNumber.L0)),
        },
        {
        (False,False): (120, (True,False,LightNumber.L0)),
            (False,True): (121, (True,False,LightNumber.L0)),
            (True,False): (122, (True,False,LightNumber.L0)),
            (True,True): (123, (True,False,LightNumber.L0)),
        },
        {
        (False,False): (124, (True,True,LightNumber.L0)),
            (False,True): (125, (True,True,LightNumber.L0)),
            (True,False): (126, (True,True,LightNumber.L0)),
            (True,True): (127, (True,True,LightNumber.L0)),
        },
        {
        (False,False): (124, (True,True,LightNumber.L0)),
            (False,True): (125, (True,True,LightNumber.L0)),
            (True,False): (126, (True,True,LightNumber.L0)),
            (True,True): (127, (True,True,LightNumber.L0)),
        },
        {
        (False,False): (124, (True,True,LightNumber.L0)),
            (False,True): (125, (True,True,LightNumber.L0)),
            (True,False): (126, (True,True,LightNumber.L0)),
            (True,True): (127, (True,True,LightNumber.L0)),
        },
        {
        (False,False): (124, (True,True,LightNumber.L0)),
            (False,True): (125, (True,True,LightNumber.L0)),
            (True,False): (126, (True,True,LightNumber.L0)),
            (True,True): (127, (True,True,LightNumber.L0)),
        },
        {
        (False,False): (124, (True,True,LightNumber.L0)),
            (False,True): (125, (True,True,LightNumber.L0)),
            (True,False): (126, (True,True,LightNumber.L0)),
            (True,True): (127, (True,True,LightNumber.L0)),
        },
        {
        (False,False): (0, (False,False,LightNumber.L0)),
            (False,True): (1, (False,False,LightNumber.L0)),
            (True,False): (2, (False,False,LightNumber.L0)),
            (True,True): (3, (False,False,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (0, (False,False,LightNumber.L0)),
            (False,True): (1, (False,False,LightNumber.L0)),
            (True,False): (2, (False,False,LightNumber.L0)),
            (True,True): (3, (False,False,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (4, (False,True,LightNumber.L0)),
            (False,True): (5, (False,True,LightNumber.L0)),
            (True,False): (6, (False,True,LightNumber.L0)),
            (True,True): (7, (False,True,LightNumber.L0)),
        },
        {
        (False,False): (0, (False,False,LightNumber.L0)),
        },
        ]
    
    def move(self, button1, button2):
        try:
            table = self._table[self._state]
            newState,res = table[(button1,button2)]
            self._state = newState
            return { "state1": res[0],
              "state2": res[1],
              "light": res[2],
            }
        
        except IndexError:
            raise Exception("Unrecognized internal state: " + str(self._state))
        
        except Exception:
            self._error(button1,button2)
    
    def _error(self, button1, button2):
        raise ValueError("Unrecognized input: " + ( "button1 = {button1}; "
                                                        "button2 = {button2}; ").format( button1=button1,
                                                        button2=button2))