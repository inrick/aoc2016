OCB_FLAGS := -use-ocamlfind
OCB := ocamlbuild $(OCB_FLAGS)

TARGET := main

.PHONY: all
all: native

.PHONY: clean
clean:
	$(OCB) -clean

.PHONY: native
native:
	$(OCB) $(TARGET).native

.PHONY: byte
byte:
	$(OCB) $(TARGET).byte

.PHONY: profile
profile:
	$(OCB) -tag profile $(TARGET).native

.PHONY: debug
debug:
	$(OCB) -tag debug $(TARGET).byte

.PHONY: test
test:
	$(OCB) -tag debug test.native && OCAMLRUNPARAM=b ./test.native
