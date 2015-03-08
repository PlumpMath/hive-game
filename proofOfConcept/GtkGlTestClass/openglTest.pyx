#!/usr/bin/python
# -*- coding: utf-8 -*-

# rotating-square.py
# A simple animation to demonstrate GtkGLExt.
#
# Copyright © 2012  B. Clausius <barcc@gmx.de>
# This program is in the public domain and you are using it at your own risk.
#
# This program is a port of rotating-square.c by
# Alif Wahid <awah005@users.sourceforge.net> and
# Naofumi Yasufuku  <naofumi@users.sourceforge.net>

from __future__ import print_function

import cython
cimport cython

import numpy as np
cimport numpy as np

import npGLMath as npGLM
cimport npGLMath as npGLM

import sys

from gi.repository import Gtk, Gdk, GLib
from gi.repository import GtkGLExt, GdkGLExt


DEF DEFAULT_WIDTH = 500
DEF DEFAULT_HEIGHT = 500
DEF DEFAULT_TITLE = "Rotating Square"

cdef class GtkGlTest:


    cdef int animate
    cdef float T
    cdef int w, h
    cdef int idle_id

    cdef GLuint vertexBuffer
    cdef GLfloat[9] vertexBufferData

    cdef GLuint progID
    cdef GLuint vertexPositionModelspaceID
    cdef GLuint MVP
    cdef np.ndarray M, V, P

    
    def __cinit__ (self):
        self.animate = True
        self.T = 0.
        self.idle_id = 0
        self.vertexBufferData  = [
            -1, -1, -1,
            1, -1, -1,
            0, 1, -1,
            ]
        self.M = npGLM.identity_matrix()
        self.V = npGLM.identity_matrix()
        self.P = npGLM.identity_matrix()

    def loadShaders (GtkGlTest self):
        cdef char * vertexShader = """
#version 120

// Input vertex data, different for all executions of this shader.

attribute vec3 vertexPosition_modelspace;
uniform mat4 MVP;

void main(){
        gl_Position = MVP*vec4(vertexPosition_modelspace, 1.0);
}
"""

        cdef char * fragmentShader = """
#version 120

void main()

{
        // Output color = red 
        gl_FragColor = vec4(1,0,0,1);
}

"""
        vertexShaderID = glCreateShader(GL_VERTEX_SHADER)
        fragmentShaderID= glCreateShader(GL_FRAGMENT_SHADER)
        glShaderSource(vertexShaderID, 1, <const char **>&vertexShader, NULL)
        glCompileShader(vertexShaderID)
        glShaderSource(fragmentShaderID, 1, <const char **>&fragmentShader, NULL)
        glCompileShader(fragmentShaderID)

        self.progID = glCreateProgram()
        glAttachShader(self.progID, vertexShaderID)
        glAttachShader(self.progID, fragmentShaderID)
        glLinkProgram(self.progID)

        glDeleteShader(vertexShaderID)
        glDeleteShader(fragmentShaderID)


    # The following section contains all the callback function definitions.

    def realize(GtkGlTest self, widget, data):
        '''The "realize" signal handler. All the OpenGL initialization
        should be performed here, such as default background colour,
        certain states etc.'''

        # OpenGL BEGIN
        if not GtkGLExt.widget_begin_gl(widget):
            return

        glewInit()

        self.loadShaders()
        self.vertexPositionModelspaceID = glGetAttribLocation(self.progID,
                "vertexPosition_modelspace")
        self.MVP = glGetUniformLocation(self.progID, "MVP")


        glGenBuffers(1, &(self.vertexBuffer))
        glBindBuffer(GL_ARRAY_BUFFER, self.vertexBuffer)
        glBufferData(GL_ARRAY_BUFFER, sizeof(self.vertexBufferData), self.vertexBufferData,
                GL_STATIC_DRAW)

        glClearColor(0, 0, 0.4, 1)
        
        self.V = npGLM.scale_matrix(.1)
        
        GtkGLExt.widget_end_gl(widget, False)
        # OpenGL END

    def configure_event(GtkGlTest self, widget, event, data):
        '''The "configure_event" signal handler. Any processing required when
        the OpenGL-capable drawing area is re-configured should be done here.
        Almost always it will be used to resize the OpenGL viewport when
        the window is resized.'''

        allocation = widget.get_allocation()
        self.w = allocation.width
        self.h = allocation.height

        # OpenGL BEGIN
        if not GtkGLExt.widget_begin_gl(widget):
            return False

        glViewport(0,0,self.w,self.h)
        
        GtkGLExt.widget_end_gl(widget, False)
        # OpenGL END

        return True

    def draw (GtkGlTest self, widget, cr, data):
        '''The "draw" signal handler. All the OpenGL re-drawing should
        be done here. This is repeatedly called as the painting routine
        every time the 'draw' event is signalled.'''

        # OpenGL BEGIN
        if not GtkGLExt.widget_begin_gl(widget):
            return False

        glClear(GL_COLOR_BUFFER_BIT)

        glUseProgram(self.progID)

        
        cdef np.ndarray[GLfloat, ndim=2, mode="fortran", cast=True] MVP = np.asfortranarray(self.P.dot(self.V).dot(self.M).astype(np.float32))
        print(self.V)
        print(self.M)
        print(MVP)
        print(MVP.dot(np.array([self.vertexBufferData[0], self.vertexBufferData[1], self.vertexBufferData[2],1])))
        print(MVP.dot(np.array([self.vertexBufferData[3], self.vertexBufferData[4], self.vertexBufferData[5],1])))
        print(MVP.dot(np.array([self.vertexBufferData[6], self.vertexBufferData[7], self.vertexBufferData[8],1])))
        glUniformMatrix4fv(self.MVP, 1, GL_FALSE, &MVP[0,0])
        
        glEnableVertexAttribArray(self.vertexPositionModelspaceID)
        glBindBuffer(GL_ARRAY_BUFFER, self.vertexBuffer)
        glVertexAttribPointer(
                   self.vertexPositionModelspaceID,
                   3,
                   GL_FLOAT,
                   GL_FALSE,
                   0,
                   <void*>0
                )
        glDrawArrays(GL_TRIANGLES, 0, 3)
        glDisableVertexAttribArray(self.vertexPositionModelspaceID)

        GtkGLExt.widget_end_gl(widget, True)
        # OpenGL END

        return True

    def idle(GtkGlTest self, widget):
        '''The idle function. Often in animations,
        idle functions are suitable for continous
        frame updates.'''

        window = widget.get_window()
        allocation = widget.get_allocation()

        self.T += 2.
        if self.T > 200.:
            self.T -= 200.
            
        x = -10+(self.T/10)
        y = 0
        
        print("%f, %f"%(x, y))
        
        self.M = npGLM.rotation_matrix((self.T/100)*3.14159265, np.array([0,0,1]))
        self.M = npGLM.translation_matrix(np.array([x, y, 0])).dot(self.M)

        # Invalidate the whole window.
        window.invalidate_rect(allocation, False)

        # Update synchronously.
        window.process_updates(False)

        return True

    def motion_notify_event(GtkGlTest self, widget, event, data):
        '''The "motion_notify_event" signal handler. Any processing required when
        the OpenGL-capable drawing area is under drag motion should be done here.'''

        # Fill in the details here.
        return False

    def button_press_event(GtkGlTest self, widget, event, data):
        '''The "button_press_event" signal handler. Any processing required when
        mouse buttons (only left and middle buttons) are pressed on the OpenGL-
        capable drawing area should be done here.

        Starts/Stops animation according mouse button clicks.
        '''
        if event.button == 1:
            self.toggle_animation(widget)
            return True
        return False

    def button_press_event_popup_menu(GtkGlTest self, widget, event):
        '''For popup menu.'''

        if event.button == 3:
            # Popup menu.
            widget.popup(None, None, None, None, event.button, event.time)
            return True
        return False

    def key_press_event(GtkGlTest self, widget, event):
        '''The "key_press_event" signal handler. Any processing required when key
        presses occur should be done here.'''

        window = widget.get_window()
        allocation = widget.get_allocation()

        if event.keyval == Gdk.KEY_Left:
            # Rotate left.
            if not self.animate:
                self.T += 2.
                if self.T > 200.:
                    self.T -= 200.
                window.invalidate_rect(allocation, False)
        elif event.keyval == Gdk.KEY_Right:
            # Rotate right.
            if not self.animate:
                self.T -= 2.
                if self.T < 0.:
                    self.T += 200.
                window.invalidate_rect(allocation, False)
        elif event.keyval == Gdk.KEY_a:
            # Toggle animation.
            self.toggle_animation(widget)
        elif event.keyval == Gdk.KEY_Escape:
            # Quit.
            Gtk.main_quit()
        else:
            return False

        return True

    def unrealize(GtkGlTest self, widget, data):
        '''The "unrealize" signal handler. Any processing required when
        the OpenGL-capable window is unrealized should be done here.'''

        # Fill in the details here
        glDeleteBuffers(1, &self.vertexBuffer)
        glDeleteProgram(self.progID)


    #
    # The following section contains the idle function management routines.
    #

    # Helper functions to add or remove the idle function.

    def idle_add(GtkGlTest self, widget):
        if self.idle_id == 0:
            #idle_id = GLib.idle_add(idle, widget, priority=Gdk.PRIORITY_REDRAW+100)
            self.idle_id = GLib.timeout_add(8, self.idle, widget, priority=Gdk.PRIORITY_REDRAW+100)

    def idle_remove(GtkGlTest self, widget):
        if self.idle_id != 0:
            GLib.source_remove(self.idle_id)
            self.idle_id = 0

    def map_event(GtkGlTest self, widget, event, data):
        '''The "map_event" signal handler. Any processing required when the
        OpenGL-capable drawing area is mapped should be done here.'''

        if self.animate:
            self.idle_add(widget)
        return True

    def unmap_event(GtkGlTest self, widget, event, data):
        '''The "unmap_event" signal handler. Any processing required when the
        OpenGL-capable drawing area is unmapped should be done here.'''

        self.idle_remove(widget)
        return True

    def visibility_notify_event(GtkGlTest self, widget, event, data):
        '''The "visibility_notify_event" signal handler. Any processing required
        when the OpenGL-capable drawing area is visually obscured should be
        done here.'''

        if self.animate:
            if event.state == Gdk.VisibilityState.FULLY_OBSCURED:
                self.idle_remove(widget)
            else:
                self.idle_add(widget)
        return True


    #
    #  The following section contains some miscellaneous utility functions.
    #

    def toggle_animation(GtkGlTest self, widget):
        '''Toggle animation.'''
        self.animate = not self.animate

        if self.animate:
            self.idle_add(widget)
        else:
            self.idle_remove(widget)
            allocation = widget.get_allocation()
            widget.get_window().invalidate_rect(allocation, False)


    #
    # The following section contains the GUI building function definitions.
    #

    def create_popup_menu(GtkGlTest self, drawing_area):
        '''Creates the popup menu to be displayed.'''

        menu = Gtk.Menu()

        # Toggle animation
        menu_item = Gtk.MenuItem.new_with_label("Toggle Animation")
        menu.append(menu_item)
        menu_item.connect_object("activate", self.toggle_animation, drawing_area)
        menu_item.show()

        # Quit
        menu_item = Gtk.MenuItem.new_with_label("Quit")
        menu.append(menu_item)
        menu_item.connect("activate", Gtk.main_quit)
        menu_item.show()

        return menu

    def create_window(GtkGlTest self, glconfig):
        '''Creates the simple application window with one
        drawing area that has an OpenGL-capable visual.'''

        ## Top-level window.

        window = Gtk.Window(Gtk.WindowType.TOPLEVEL)
        window.set_title(DEFAULT_TITLE)

        # Get automatically redrawn if any of their children changed allocation.
        window.set_reallocate_redraws(True)

        # Connect signal handlers to the window
        window.connect("delete_event", Gtk.main_quit)

        ## VBox.

        vbox = Gtk.VBox(False, 0)
        window.add(vbox)
        vbox.show()

        ## Drawing area to draw OpenGL scene.

        drawing_area = Gtk.DrawingArea()
        drawing_area.set_size_request(DEFAULT_WIDTH, DEFAULT_HEIGHT)

        # Set OpenGL-capability to the widget
        GtkGLExt.widget_set_gl_capability(drawing_area,
                      glconfig,
                      None,
                      True,
                      GdkGLExt.RenderType.RGBA_TYPE)
        drawing_area.add_events(
                   Gdk.EventMask.BUTTON1_MOTION_MASK    |
                   Gdk.EventMask.BUTTON2_MOTION_MASK    |
                   Gdk.EventMask.BUTTON_PRESS_MASK      |
                   Gdk.EventMask.VISIBILITY_NOTIFY_MASK)

        # Connect signal handlers to the drawing area
        drawing_area.connect_after("realize", self.realize, None)
        drawing_area.connect("configure_event", self.configure_event, None)
        drawing_area.connect("draw", self.draw, None)

        drawing_area.connect("motion_notify_event", self.motion_notify_event, None)
        drawing_area.connect("button_press_event", self.button_press_event, None)
        drawing_area.connect("unrealize", self.unrealize, None)

        # key_press_event handler for top-level window
        window.connect_object("key_press_event", self.key_press_event, drawing_area)

        # For idle function.
        drawing_area.connect("map_event", self.map_event, None)
        drawing_area.connect("unmap_event", self.unmap_event, None)
        drawing_area.connect("visibility_notify_event", self.visibility_notify_event, None)

        vbox.pack_start(drawing_area, True, True, 0)
        drawing_area.show()

        # Popup menu.

        menu = self.create_popup_menu(drawing_area)
        drawing_area.connect_object("button_press_event", self.button_press_event_popup_menu, menu)

        ## Simple quit button.

        button = Gtk.Button.new_with_label("Quit")
        button.connect("clicked", Gtk.main_quit)
        vbox.pack_start(button, False, False, 0)
        button.show()

        return window


    #
    # The following section contains utility function definitions.
    #

    def configure_gl(GtkGlTest self):
        '''Configure the OpenGL framebuffer.'''

        # Try double-buffered visual
        try:
            glconfig = GdkGLExt.Config.new_by_mode(GdkGLExt.ConfigMode.RGBA |
                                                   GdkGLExt.ConfigMode.DEPTH |
                                                   GdkGLExt.ConfigMode.DOUBLE)
        except TypeError:
            print("*** Cannot find the double-buffered visual.")
            print("*** Trying single-buffered visual.")

            # Try single-buffered visual
            try:
                glconfig = GdkGLExt.Config.new_by_mode(GdkGLExt.ConfigMode.RGBA |
                                                       GdkGLExt.ConfigMode.DEPTH)
            except TypeError:
                print("*** No appropriate OpenGL-capable visual found.")
                sys.exit(1)

        return glconfig


    #
    # The main function is rather trivial.
    #

    def main(GtkGlTest self):
        # Configure OpenGL framebuffer.
        glconfig = self.configure_gl()

        # Create and show the application window.
        window = self.create_window(glconfig)
        window.show()

        Gtk.main()
        return 0


