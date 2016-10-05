OUTFILE:=static/out.js

all:
	pulp build -O | closure -O SIMPLE > $(OUTFILE)

init:
	bower i

dist-clean:
	$(RM) -rf bower_components output $(OUTFILE)
