DHCP_SERVER_BIN=bin/conet-dhcp-server
DHCP_SERVER_SRC=dhcp-server/
RM=rm -rf

all: $(DHCP_SERVER_BIN)

$(DHCP_SERVER_BIN): force_look
	$(MAKE) -C $(DHCP_SERVER_SRC) $(MFLAGS)

force_look:
	true

clean:
	-$(RM) $(DHCP_SERVER_BIN)

.PHONY: clean
