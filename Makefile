#
# Thanks to: https://github.com/chef/mini_s3
#

#
# Use rebar3 from either:
# - ./rebar3
# - rebar3 on the PATH (found via which)
# - Downloaded from $REBAR3_URL
#

.PHONY: all compile test dialyzer dialyze clean distclean shell docs edoc

REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

ifeq ($(wildcard rebar3),rebar3)
  REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = rebar3
endif

all: compile

compile: $(REBAR3)
	@$(REBAR3) compile

test:
	@$(REBAR3) eunit

dialyzer:
	@$(REBAR3) dialyzer

dialyze: dialyzer

clean:
	@$(REBAR3) clean

distclean:
	@rm -rf _build

shell:
	@$(REBAR3) shell

docs:
	@$(REBAR3) edoc

edoc: docs

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3
