# Code to bring engine and graphics subsystems goes here.

from . import graphics, engine

engine = engine.Engine() # subclass of Thread?

graphics = graphics.GUI(engine)

engine.start()

graphics.show()

