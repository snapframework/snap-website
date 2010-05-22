VERSION=0.1.2

all:
	rm -f static/docs/latest
	mkdir -p static/docs/${VERSION}
	ln -s ${VERSION} static/docs/latest
