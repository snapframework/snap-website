VERSION=0.2.4

all:
	rm -f static/docs/latest
	mkdir -p static/docs/${VERSION}
	ln -s ${VERSION} static/docs/latest
