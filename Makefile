DHCP_SERVER_BIN=bin/conet-dhcp-server
DHCP_SERVER_SRC=dhcp-server/

all: $(DHCP_SERVER_BIN)

$(DHCP_SERVER_BIN): force_look
	$(MAKE) -C $(DHCP_SERVER_SRC) $(MFLAGS)

force_look:
	true

clean:
	$(MAKE) -C $(DHCP_SERVER_SRC) clean

.PHONY: clean
