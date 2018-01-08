# -*- coding: utf-8 -*-
import pygame
from pygame.locals import *
from time import sleep
from Controller import Controller as Ca_4
from pygame import Rect

ctrl = Ca_4()

BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
PURPLE = (120, 78, 240)
RED = (255, 0, 0)
BLUE = (0, 0, 255)
GREEN = (0, 200, 0)

POS_LIST = []
N_LAYERS = 2
rect = Rect((0,0,0,0))

def return_input(uav1_pos, uav2_pos, uav3_pos, uav1_2_collide, uav1_3_collide,
                uav2_3_collide, uav1_layer, uav2_layer, uav3_layer):
    return {
        "uav1_pos": Ca_4.Pos.NONE if uav1_pos == 0 else Ca_4.Pos.A,
        "uav2_pos": Ca_4.Pos.NONE if uav2_pos == 0 else Ca_4.Pos.A,
        "uav3_pos": Ca_4.Pos.NONE if uav3_pos == 0 else Ca_4.Pos.A,
        "uav1_2_collide": False,# uav1_2_collide,
        "uav1_3_collide": False,#uav1_3_collide,
        "uav2_3_collide": False,#uav2_3_collide,
        "uav1_layer": Ca_4.Layer.FIRST if uav1_layer == 0 else
        Ca_4.Layer.SECOND,
        "uav2_layer": Ca_4.Layer.FIRST if uav2_layer == 0 else
        Ca_4.Layer.SECOND,
        "uav3_layer": Ca_4.Layer.FIRST if uav3_layer == 0 else
        Ca_4.Layer.SECOND,
    }

def is_collide(uav1, uav2):
    return False if uav1.region_rect_1.collidelist([uav2.region_rect_1]) == -1\
                and uav1.region_rect_2.collidelist([uav2.region_rect_2]) == -1\
                 else True

def is_in_or(op_reg, point):
    return op_reg.collidepoint(point)

def is_in(loc1, loc2):
    if abs(loc1[0] - loc2[0]) < 20 and abs(loc1[1] - loc2[1]) < 20:
        return True
    else:
        return False

def make_rect(initial, destination):
    if initial[0] < destination[0]:
        x = initial[0] - 10
    else:
        x = destination[0] - 10

    if initial[1] < destination[1]:
        y = initial[1] - 10
    else:
        y = destination[1] - 10
    w = abs(initial[0] - destination[0]) + 60
    l = abs(initial[1] - destination[1]) + 60
    return pygame.Rect(x, y, w, l)

def get_commands(uav, point):
    diff_x = (uav.position[0]%450) - point[0]
    diff_y = uav.position[1] - point[1]
    hor_cmd = ['east']*abs(diff_x/45) if diff_x <= 0 else ['west']*(diff_x/45)
    ver_cmd = ['south']*abs(diff_y/45) if diff_y <= 0 else ['north']*(diff_y/45)
    return hor_cmd + ver_cmd

def get_dir(loc_1, loc_2):
    if loc_1[0] > loc_2[0]:
        return 'west'
    elif loc_1[1] > loc_2[1]:
        return 'north'
    elif loc_1[1] < loc_2[1]:
        return 'south'
    elif loc_1[0] < loc_2[0]:
        return 'east'

class Circle(pygame.sprite.Sprite):
    def __init__(self, display, color, init_rect):
        pygame.sprite.Sprite.__init__(self)
        self.image = display
        self.rect = self.image.get_rect()

    def update(self,rect):
        self.image = pygame.Surface((rect[2], rect[3]))
        self.image.fill((255,255,255))
        self.rect = self.image.get_rect()
        self.rect.center = (rect[0], rect[1])
        pygame.draw.rect(self.image, BLACK, rect, 5)

class RegionSprite(pygame.sprite.Sprite):

    def __init__(self, display, position, rect):
        pygame.sprite.Sprite.__init__(self)
        self.position = position
        self.image = pygame.Surface((500,500))
        self.color = BLACK
        self.surface = display
        self.rect = self.surface.get_rect()
        pygame.draw.rect(self.surface, self.color, self.rect)

    def update(self, rect):
        self.rect.center = self.position

class PointSprite(pygame.sprite.Sprite):

    def __init__(self, position, label):
        pygame.sprite.Sprite.__init__(self)
        self.src_image = pygame.image.load("assets/point.png")
        self.src_image = pygame.transform.scale(self.src_image,(25,25))
        self.position = position
        self.chosen = False
        self.label = label

    def update(self):
        if self.chosen:
            color_surface(self, RED)
            self.image = pygame.transform.rotate(self.src_image, 0)
        else:
            color_surface(self, BLACK)
            self.image = pygame.transform.rotate(self.src_image, 0)
        self.rect = self.image.get_rect()
        self.rect.center = (self.position[0]*450 + self.position[1],
                            self.position[2])

    def select(self):
        self.chosen = True

    def unselect(self):
        self.chosen = False

class UAVSprite(pygame.sprite.Sprite):
    MAX_SPEED = 45
    TURN_SPEED = 5

    def __init__(self, position, color, display):
        pygame.sprite.Sprite.__init__(self)
        self.src_image = pygame.image.load("assets/uav.png")
        self.position = position
        self.next_state = (position[0]+(position[1]/450), (position[1]%450)/45,
                           position[2]/45)
        self.speed = self.direction = 0
        self.k_left = self.k_right = self.k_down = self.k_up = 0
        self.discrete = self.next_state
        arr = pygame.surfarray.pixels3d(self.src_image)
        arr[:,:,0] = color[0]
        arr[:,:,1] = color[1]
        arr[:,:,2] = color[2]
        self.color = color
        self.display = display
        self.region_rect_1 = (0,0,0,0)
        self.region_rect_2 = (0,0,0,0)

    def move(self, action):
        l, x, y = self.next_state
        if action == 'east' and self.next_state[1] < 9:
            x += 1
        elif action == 'west' and self.next_state[1] > 1:
            x -= 1
        elif action == 'south' and self.next_state[2] < 15:
            y += 1
        elif action == 'north' and self.next_state[2] > 1:
            y -= 1
        elif action == 'ascend' and self.next_state[0] < N_LAYERS-1:
            l += 1
            x += 0
            y += 0
        elif action == 'descend' and self.next_state[0] > 0:
            l -= 1
            x += 0
            y += 0
        self.next_state = (l, x, y)

    def update(self):
        x = self.next_state[1] * 45 + (450 * self.next_state[0])
        y = self.next_state[2] * 45
        self.position = (x, y)
        self.image = pygame.transform.rotate(self.src_image, 0)
        self.rect = self.image.get_rect()
        self.rect.center = self.position
        self.discrete = self.next_state
        pygame.draw.rect(self.display, self.color, self.region_rect_1, 2)
        pygame.draw.rect(self.display, self.color, self.region_rect_2, 2)

    def update_region(self, position, color = None):
        if True:
            initial = self.position
            rect_1 = make_rect((initial[0]%450,initial[1]),[position[1]%450,
                                      position[2]])
            rect_2 = make_rect(((initial[0]%450) + 450, initial[1]),[450 + (position[1]%450), position[2]])
            if color is not None:
                pygame.draw.rect(self.display, color, rect_1, 2)
                pygame.draw.rect(self.display, color, rect_2, 2)
            else:
                pygame.draw.rect(self.display, self.color, rect_1, 2)
                pygame.draw.rect(self.display, self.color, rect_2, 2)
            self.region_rect_1 = rect_1
            self.region_rect_2 = rect_2
        pygame.draw.rect(self.display, self.color, self.region_rect_1, 2)
        pygame.draw.rect(self.display, self.color, self.region_rect_2, 2)
    def get_layer(self):
        return self.discrete[0]

    def get_pos(self):
        for pos in POS_LIST:
            if is_in(self.rect.center, pos.rect.center):
                return pos.label
        return 0

def color_surface(surface, color):
    arr = pygame.surfarray.pixels3d(surface.src_image)
    arr[:,:,0] = color[0]
    arr[:,:,1] = color[1]
    arr[:,:,2] = color[2]

class App:
    def __init__(self):
        self._running = True
        self.display = None
        self.size = self.width, self.height = 900, 700
        self.layers = 2

    def on_init(self):
        pygame.init()
        pygame.font.init()
        myfont = pygame.font.SysFont('Comic Sans MS', 22)
        self.loc_a = myfont.render('Location A', False, (0, 0, 0))
        self.layer_1 = myfont.render('Layer 1', False, (0, 0, 0))
        self.layer_2 = myfont.render('Layer 2', False, (0, 0, 0))
        self.display = pygame.display.set_mode(self.size,
                                               pygame.HWSURFACE |
                                               pygame.DOUBLEBUF)
        self.background = pygame.Surface(self.display.get_size())
        self.background = self.background.convert()
        self.background.fill((255, 255, 255))
        self._running = True
        self.display.fill((255, 255, 255))
        pygame.draw.line(self.display, (100, 100, 100),
                         [int(self.width/self.layers), 0],
                         [int(self.width/self.layers), self.height], 5)
        rect = self.display.get_rect()
        self.uav_1 = UAVSprite((0 , 45, 45), BLACK, self.display)
        self.uav_2 = UAVSprite((0, 45,300), PURPLE,
                               self.display)
        self.uav_3 = UAVSprite((1, 350, 145), GREEN, self.display)
        self.UAV_LIST = []
        self.UAV_LIST.append(self.uav_1)
        self.UAV_LIST.append(self.uav_2)
        self.UAV_LIST.append(self.uav_3)
        self.point_1 = PointSprite((0 , 300, 300), 1)

        POS_LIST.append(self.point_1)
        self.operating_region_1 = Circle(self.display, BLACK, (100,100,200,200))
        self.region_group = pygame.sprite.RenderPlain((self.operating_region_1))
        self.uav_group = pygame.sprite.RenderPlain((self.uav_1, self.uav_2,
                                                    self.uav_3))
        self.point_group = pygame.sprite.RenderPlain(POS_LIST)
        self.layers = 2
        self.uav_group.update()
        self.uav_group.clear(self.display,self.background)
        self.uav_group.draw(self.display)

        self.point_group.update()
        self.point_group.clear(self.display,self.background)
        self.point_group.draw(self.display)

        self.uav_1.update_region((0, 150, 150))
        self.uav_2.update_region((0, 300, 300))
        self.uav_3.update_region((1, 400, 200))

        pygame.display.flip()

    def on_event(self, event):
        if event.type == pygame.QUIT:
            self._running = False

    def on_loop(self):
        pass

    def on_cleanup(self):
        pygame.quit()

    def reset_map(self):
        self.display.fill(pygame.Color("white"))
        pygame.draw.line(self.display, (100, 100, 100),
                         [int(self.width/self.layers), 0],
                    [int(self.width/self.layers), self.height], 5)
        self.display.blit(self.loc_a, (700, 320))
        self.display.blit(self.layer_1, (350,635))
        self.display.blit(self.layer_2, (800, 635))

    def show(self):
        self.reset_map()
        self.uav_group.update()
        self.uav_group.clear(self.display,self.background)
        self.uav_group.draw(self.display)

        self.point_group.update()
        self.point_group.clear(self.display,self.background)
        self.point_group.draw(self.display)
        pygame.display.flip()
        sleep(0.5)

    def on_execute(self):
        if self.on_init() == False:
            self._running = False

        while( self._running ):
            for event in pygame.event.get():
                self.on_event(event)

            self.show()

            # creating inputs to controller
            uav1_pos = self.uav_1.get_pos()
            uav2_pos = self.uav_2.get_pos()
            uav3_pos = self.uav_3.get_pos()
            uav1_2_collide = is_collide(self.uav_1, self.uav_2)
            uav1_3_collide = is_collide(self.uav_1, self.uav_3)
            uav2_3_collide = is_collide(self.uav_2, self.uav_3)
            uav1_layer = self.uav_1.get_layer()
            uav2_layer = self.uav_2.get_layer()
            uav3_layer = self.uav_3.get_layer()
            inputs = return_input(uav1_pos, uav2_pos, uav3_pos, uav1_2_collide,
                   uav1_3_collide, uav2_3_collide, uav1_layer, uav2_layer,
                   uav3_layer)

            # getting the outputs out of the controller
            outputs = ctrl.move(**inputs)
            commands = {0:[],1:[],2:[]}
            for i in range(0,3):
                if outputs['uav{}_goto'.format(i+1)] == Ca_4.Pos.A:
                    self.UAV_LIST[i].update_region(self.point_1.position)
                    self.show()
                if outputs['uav{}_action'.format(i+1)] == Ca_4.Command.ASC:
                    self.UAV_LIST[i].move('ascend')
                elif outputs['uav{}_action'.format(i+1)] == Ca_4.Command.DES:
                    self.UAV_LIST[i].move('descend')
                elif outputs['uav{}_goto'.format(i+1)] == Ca_4.Pos.A:
                    commands[i] = get_commands(self.UAV_LIST[i],
                                               (self.point_1.position[1],
                                                self.point_1.position[2]))
                if len(commands[i]) > 0:
                    for command in commands[i]:
                        self.UAV_LIST[i].move(command)
                        self.show()

        self.on_cleanup()

if __name__ == "__main__" :
    theApp = App()
    theApp.on_execute()
