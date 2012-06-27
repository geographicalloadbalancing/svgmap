SOURCES = $(shell ls $(SRC)/*.scala)
S = scala
SC = scalac
TARGET = target
CP = $(shell ls lib/*.jar):src
SPEC = scala.RomanSpec

compile: $(SOURCES:.scala=.class)

%.class: %.scala
	@echo "Compiling $*.scala.."
	@scalac -cp $(CP) -d . $*.scala

run: compile
	@scala -cp $(CP) edu.caltech.glb.svgmap.Main

clean:
	@$(RM) $(SRC)/*.class