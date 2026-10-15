SBCL ?= sbcl
SYSTEM := autolith-webui

.PHONY: all build run smoke package install-links clean

all: build

build:
	./bin/build.sh

run: build
	./bin/autolith-webui

smoke:
	$(SBCL) --noinform --non-interactive \
	  --eval "(require :asdf)" \
	  --eval "(let ((setup (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))) (when (probe-file setup) (load setup)))" \
	  --eval "(asdf:load-system :$(SYSTEM))" \
	  --eval "(unless (autolith-webui:self-test) (uiop:quit 1))"

package: build
	@version=$$(git describe --tags --always 2>/dev/null || echo dev); \
	mkdir -p dist; \
	tar -czf dist/autolith-webui-$$version.tar.gz \
	  -C .. \
	  --exclude=autolith-webui/.git \
	  --exclude=autolith-webui/bin/autolith-webui \
	  --exclude=autolith-webui/dist \
	  autolith-webui; \
	echo "packaged dist/autolith-webui-$$version.tar.gz"

install-links:
	mkdir -p $(HOME)/quicklisp/local-projects
	ln -sfn $(CURDIR) $(HOME)/quicklisp/local-projects/autolith-webui

clean:
	rm -f bin/autolith-webui
	rm -rf dist
