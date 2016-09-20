PACKAGENAME=dbus
COLLECTS=dbus

all: setup test

clean:
	find . -name compiled -type d | xargs rm -rf

setup:
	raco setup $(COLLECTS)

test:
	raco test -p $(PACKAGENAME)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)
