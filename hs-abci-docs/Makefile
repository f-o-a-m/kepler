# Thank you Apple
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	SED=sed -i''
endif
ifeq ($(UNAME_S),Darwin)
	SED=sed -i ''
endif

pre-process:
	find ./doc/ -type f -name "*.md" -exec $(SED) -e 's/~~~ haskell.*/```haskell/g' {} + && \
	find ./doc/ -type f -name "*.md" -exec $(SED) -e 's/~~~/```/g' {} +

tintin:
	tintin run
