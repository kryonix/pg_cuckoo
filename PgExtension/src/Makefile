# Module big enables the use of multiple source files
MODULE_big = cuckoo
EXTENSION = cuckoo
DATA = cuckoo--1.0.sql

#OBJS = $(patsubst %.c,%.o,$(wildcard *.c))
OBJS = cuckoo.o
PG_CONFIG = pg_config
PGXS := $(shell $(PG_CONFIG) --pgxs)
include $(PGXS)

ifeq ($(PORTNAME), darwin)
override CFLAGS += -undefined dynamic_lookup
endif

#override CFLAGS += -I$(top_builddir)/src/pl/plpgsql/src