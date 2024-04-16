library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity myManager_v1_0_M00_AXI is
	generic (
		-- Users to add parameters here
		-- User parameters ends
		-- Do not modify the parameters beyond this line
	-- The master requires a target slave base address.
    -- The master will initiate read and write transactions on the slave with base address specified here as a parameter.
		C_M_PS2_SLAVE_BASE_ADDR	: std_logic_vector	:= x"44A00000";
		C_M_VIDEOMEM_BASE_ADDR	: std_logic_vector	:= x"44A10000";
	-- Width of M_AXI address bus. 
    -- The master generates the read and write addresses of width specified as C_M_AXI_ADDR_WIDTH.
		C_M_AXI_ADDR_WIDTH	: integer	:= 32;
		-- Width of M_AXI data bus. 
    -- The master issues write data and accept read data where the width of the data bus is C_M_AXI_DATA_WIDTH
		C_M_AXI_DATA_WIDTH	: integer	:= 32
		);
	port (
		-- Users to add ports here
        IRQ_I : in std_logic;
		-- User ports ends
		-- Do not modify the ports beyond this line
			-- AXI clock signal
		M_AXI_ACLK	: in std_logic;
		-- AXI active low reset signal
		M_AXI_ARESETN	: in std_logic;
		-- Master Interface Write Address Channel ports. Write address (issued by master)
		M_AXI_AWADDR	: out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
		-- Write channel Protection type.
    -- This signal indicates the privilege and security level of the transaction,
    -- and whether the transaction is a data access or an instruction access.
		M_AXI_AWPROT	: out std_logic_vector(2 downto 0);
		-- Write address valid. 
    -- This signal indicates that the master signaling valid write address and control information.
		M_AXI_AWVALID	: out std_logic;
		-- Write address ready. 
    -- This signal indicates that the slave is ready to accept an address and associated control signals.
		M_AXI_AWREADY	: in std_logic;
		-- Master Interface Write Data Channel ports. Write data (issued by master)
		M_AXI_WDATA	: out std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
		-- Write strobes. 
    -- This signal indicates which byte lanes hold valid data.
    -- There is one write strobe bit for each eight bits of the write data bus.
		M_AXI_WSTRB	: out std_logic_vector(C_M_AXI_DATA_WIDTH/8-1 downto 0);
		-- Write valid. This signal indicates that valid write data and strobes are available.
		M_AXI_WVALID	: out std_logic;
		-- Write ready. This signal indicates that the slave can accept the write data.
		M_AXI_WREADY	: in std_logic;
		-- Master Interface Write Response Channel ports. 
    -- This signal indicates the status of the write transaction.
		M_AXI_BRESP	: in std_logic_vector(1 downto 0);
		-- Write response valid.
    -- This signal indicates that the channel is signaling a valid write response
		M_AXI_BVALID	: in std_logic;
		-- Response ready. This signal indicates that the master can accept a write response.
		M_AXI_BREADY	: out std_logic;
		-- Master Interface Read Address Channel ports. Read address (issued by master)
		M_AXI_ARADDR	: out std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
		-- Protection type. 
    -- This signal indicates the privilege and security level of the transaction, 
    -- and whether the transaction is a data access or an instruction access.
		M_AXI_ARPROT	: out std_logic_vector(2 downto 0);
		-- Read address valid. 
    -- This signal indicates that the channel is signaling valid read address and control information.
		M_AXI_ARVALID	: out std_logic;
		-- Read address ready. 
    -- This signal indicates that the slave is ready to accept an address and associated control signals.
		M_AXI_ARREADY	: in std_logic;
		-- Master Interface Read Data Channel ports. Read data (issued by slave)
		M_AXI_RDATA	: in std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
		-- Read response. This signal indicates the status of the read transfer.
		M_AXI_RRESP	: in std_logic_vector(1 downto 0);
		-- Read valid. This signal indicates that the channel is signaling the required read data.
		M_AXI_RVALID	: in std_logic;
		-- Read ready. This signal indicates that the master can accept the read data and response information.
		M_AXI_RREADY	: out std_logic
	);
end myManager_v1_0_M00_AXI;

architecture implementation of myManager_v1_0_M00_AXI is
   
    component char_lookup_table is
        Port ( clk : in std_logic;
               reset : in std_logic;
               ascii : in std_logic_vector(7 downto 0);
               line : in integer range 0 to 11;
               pixels : out std_logic_vector(7 downto 0));
    end component;
    component scancode2ascii is
        Port ( scancode : in std_logic_vector(7 downto 0);
               ascii : out std_logic_vector(7 downto 0);
                  shift, ctrl, alt : in std_logic
                 );
    end component;
    --signal ascii : std_logic_vector(7 downto 0);
    --signal line : integer range 0 to 11;
    --signal pixels : std_logic_vector(7 downto 0);
	signal code, ascii : std_logic_vector(7 downto 0);
	signal shift, ctrl, alt : std_logic;

    signal current_addr : std_logic_vector(31 downto 0);
		 type state is ( INITMEM,   -- Clear the memory
	                 WAIT4IRQ,  -- Wait for interrupt from PS/2
	 				 WAIT4CODE,	-- Read scancode from PS/2
	 				 GETPIXELS1,-- Wait for pixel data to be ready
	 				 GETPIXELS2,-- Wait for pixel data to be ready
                     WAIT4MEMACK);-- write pixels to memory 
	 signal mst_exec_state  : state ; 
	constant CHARS_PER_LINE : integer := 80;
	constant LINES_PER_PAGE : integer := 40;
	constant CHARS_PER_PAGE : integer := CHARS_PER_LINE * LINES_PER_PAGE;
	constant CHAR_WIDTH : integer := 8;
	constant CHAR_HEIGHT : integer := 12;
	constant PIXELS_PER_WORD : integer := 8;
	constant BITS_PER_PIXEL : integer := 4;

	-- TRANS_NUM_BITS is the width of the index counter for
	-- number of write or read transaction..
--	 constant  TRANS_NUM_BITS  : integer := clogb2(C_M_TRANSACTIONS_NUM-1);
    
	signal txtcolor, bgcolor : std_logic_vector(BITS_PER_PIXEL-1 downto 0);
	signal pixels : std_logic_vector(CHAR_WIDTH-1 downto 0);
	signal reg_pixels : std_logic_vector(CHAR_WIDTH-1 downto 0);
	signal color_pixels : std_logic_vector(31 downto 0);
	signal current_char : integer range 0 to CHARS_PER_LINE-1;
	signal current_line : integer range 0 to LINES_PER_PAGE-1;
	signal scan_line : integer range 0 to CHAR_HEIGHT;
	signal pixelnum : integer range 0 to CHAR_WIDTH-1;
	
	signal rst_p : std_logic;

	-- AXI4LITE signals
	--write address valid
	signal axi_awvalid	: std_logic;
	--write data valid
	signal axi_wvalid	: std_logic;
	--read address valid
	signal axi_arvalid	: std_logic;
	--read data acceptance
	signal axi_rready	: std_logic;
	--write response acceptance
	signal axi_bready	: std_logic;
	--write address
	signal axi_awaddr	: std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
	--write data
	signal axi_wdata	: std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
	--read addresss
	signal axi_araddr	: std_logic_vector(C_M_AXI_ADDR_WIDTH-1 downto 0);
	--Asserts when there is a write response error
	signal write_resp_error	: std_logic;
	--Asserts when there is a read response error
	signal read_resp_error	: std_logic;
	--A pulse to initiate a write transaction
	signal start_single_write	: std_logic;
	--A pulse to initiate a read transaction
	signal start_single_read	: std_logic;
	--Asserts when a single beat write transaction is issued and remains asserted till the completion of write trasaction.
	signal write_issued	: std_logic;
	--Asserts when a single beat read transaction is issued and remains asserted till the completion of read trasaction.
	signal read_issued	: std_logic;
	--flag that marks the completion of write trasactions. The number of write transaction is user selected by the parameter C_M_TRANSACTIONS_NUM.
	signal writes_done	: std_logic;
	--flag that marks the completion of read trasactions. The number of read transaction is user selected by the parameter C_M_TRANSACTIONS_NUM
	signal reads_done	: std_logic;
	--The error register is asserted when any of the write response error, read response error or the data mismatch flags are asserted.
	signal error_reg	: std_logic;
	--index counter to track the number of write transaction issued
--	signal write_index	: std_logic_vector(TRANS_NUM_BITS downto 0);
	--index counter to track the number of read transaction issued
--	signal read_index	: std_logic_vector(TRANS_NUM_BITS downto 0);
	--Expected read data used to compare with the read data.
	signal expected_rdata	: std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
	--Flag marks the completion of comparison of the read data with the expected read data
	signal compare_done	: std_logic;
	--This flag is asserted when there is a mismatch of the read data with the expected read data.
	signal read_mismatch	: std_logic;
	--Flag is asserted when the write index reaches the last write transction number
	signal last_write	: std_logic;
	--Flag is asserted when the read index reaches the last read transction number
	signal last_read	: std_logic;
	signal init_txn_ff	: std_logic;
	signal init_txn_ff2	: std_logic;
	signal init_txn_edge	: std_logic;
	signal init_txn_pulse	: std_logic;
	
	signal reg_rdata	: std_logic_vector(C_M_AXI_DATA_WIDTH-1 downto 0);
	
--begin
--    -- Read scancode when interrupt is asserted
--    process (ps2_interrupt)
--    begin
--        if rising_edge(ps2_interrupt) then
--            scancode <= ps2_data;
--            -- Read the flags for ctrl, shift, and alt keys (example)
--            temp_ctrl_flag <= ps2_data(0);
--            temp_shift_flag <= ps2_data(1);
--            temp_alt_flag <= ps2_data(2);
--        end if;
--    end process;

--    -- Pass scancode to scancode2ascii module
--    scancode_conversion: scancode2ascii
--        port map (
--            scancode => scancode,
--            ascii_code => ascii_code
--        );

--    -- Assign flags based on temporary values
--    ctrl_flag <= temp_ctrl_flag;
--    shift_flag <= temp_shift_flag;
--    alt_flag <= temp_alt_flag;

    -- Generate pixel data based on ASCII code (example)
--    process (ascii_code)
--    begin
--        case ascii_code is
--            when "00000001" => -- Example: generate pixel data for character 'A'
--                pixel_data <= "000000000010"; -- Example pixel data for 'A'
--            when others =>
--                pixel_data <= (others => '0');
--        end case;
--    end process;

	-- I/O Connections assignments

	current_addr <= std_logic_vector(unsigned(C_M_VIDEOMEM_BASE_ADDR) + 
                  (unsigned(current_line) * CHARS_PER_LINE * CHAR_HEIGHT + 
                  unsigned(current_char) + unsigned(scan_line) * CHARS_PER_LINE) * 
                  CHAR_WIDTH / PIXELS_PER_WORD);
                  --slave write address
	M_AXI_AWADDR	<= current_addr;

	--Adding the offset address to the base addr of the slave
	M_AXI_ARADDR	<= std_logic_vector(unsigned(C_M_PS2_SLAVE_BASE_ADDR));
--	M_AXI_AWADDR	<= std_logic_vector (unsigned(C_M_TARGET_SLAVE_BASE_ADDR) + unsigned(axi_awaddr));
	--AXI 4 write data
	M_AXI_WDATA	<= axi_wdata;
	M_AXI_AWPROT	<= "000";
	M_AXI_AWVALID	<= axi_awvalid;
	--Write Data(W)
	M_AXI_WVALID	<= axi_wvalid;
	--Set all byte strobes in this example
	M_AXI_WSTRB	<= "1111";
	--Write Response (B)
	M_AXI_BREADY	<= axi_bready;
	--Read Address (AR)
	M_AXI_ARADDR	<= std_logic_vector(unsigned(C_M_PS2_SLAVE_BASE_ADDR));
--	M_AXI_ARADDR	<= std_logic_vector(unsigned(C_M_TARGET_SLAVE_BASE_ADDR) + unsigned(axi_araddr));
	M_AXI_ARVALID	<= axi_arvalid;
	M_AXI_ARPROT	<= "001";
	--Read and Read Response (R)
	M_AXI_RREADY	<= axi_rready;
	--Example design I/O
--	TXN_DONE	<= compare_done;
	init_txn_pulse	<= ( not init_txn_ff2)  and  init_txn_ff;
	
	--Generate a pulse to initiate AXI transaction.
--	process(M_AXI_ACLK)                                                          
--	begin                                                                             
--	  if (rising_edge (M_AXI_ACLK)) then                                              
--	      -- Initiates AXI transaction delay        
--	    if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                
--	      init_txn_ff <= '0';                                                   
--	        init_txn_ff2 <= '0';                                                          
--	    else                                                                                       
--	      init_txn_ff <= INIT_AXI_TXN;
--	        init_txn_ff2 <= init_txn_ff;                                                                     
--	    end if;                                                                       
--	  end if;                                                                         
--	end process; 

	  process(M_AXI_ACLK)                                                          
	  begin                                                                             
	    if (rising_edge (M_AXI_ACLK)) then                                              
	      --Only VALID signals must be deasserted during reset per AXI spec             
	      --Consider inverting then registering active-low reset for higher fmax        
	       if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                               
	        axi_awvalid <= '0';                                                         
	      else                                                                          
	        --Signal a new address/data command is available by user logic              
	        if (start_single_write = '1') then                                          
	          axi_awvalid <= '1';                                                       
	        elsif (M_AXI_AWREADY = '1' and axi_awvalid = '1') then                      
	          --Address accepted by interconnect/slave (issue of M_AXI_AWREADY by slave)
	          axi_awvalid <= '0';                                                       
	        end if;                                                                     
	      end if;                                                                       
	    end if;                                                                         
	  end process;                                                                      
                                                             
--	  process(M_AXI_ACLK)                                                               
--	  begin                                                                             
--	    if (rising_edge (M_AXI_ACLK)) then                                              
--	     if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                 
--	        write_index <= (others => '0');                                             
--	      elsif (start_single_write = '1') then                                         
--	        -- Signals a new write address/ write data is                               
--	        -- available by user logic                                                  
--	        write_index <= std_logic_vector (unsigned(write_index) + 1);                                           
--	      end if;                                                                       
--	    end if;                                                                         
--	  end process;                                                                      

	   process(M_AXI_ACLK)                                                 
	   begin                                                                         
	     if (rising_edge (M_AXI_ACLK)) then                                          
	      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                           
	         axi_wvalid <= '0';                                                      
	       else                                                                      
	         if (start_single_write = '1') then                                      
	           --Signal a new address/data command is available by user logic        
	           axi_wvalid <= '1';                                                    
	         elsif (M_AXI_WREADY = '1' and axi_wvalid = '1') then                    
	           --Data accepted by interconnect/slave (issue of M_AXI_WREADY by slave)
	           axi_wvalid <= '0';                                                    
	         end if;                                                                 
	       end if;                                                                   
	     end if;                                                                     
	   end process;                                                                  

	  process(M_AXI_ACLK)                                            
	  begin                                                                
	    if (rising_edge (M_AXI_ACLK)) then                                 
	      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                   
	        axi_bready <= '0';                                             
	      else                                                             
	        if (M_AXI_BVALID = '1' and axi_bready = '0') then              
	          -- accept/acknowledge bresp with axi_bready by the master    
	          -- when M_AXI_BVALID is asserted by slave                    
	           axi_bready <= '1';                                          
	        elsif (axi_bready = '1') then                                  
	          -- deassert after one clock cycle                            
	          axi_bready <= '0';                                           
	        end if;                                                        
	      end if;                                                          
	    end if;                                                            
	  end process;                                                         
	--Flag write errors                                                    
	  write_resp_error <= (axi_bready and M_AXI_BVALID and M_AXI_BRESP(1));

	  process(M_AXI_ACLK)                                                              
	  begin                                                                            
	    if (rising_edge (M_AXI_ACLK)) then                                             
	      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                 
	        read_index <= (others => '0');                                             
	      else                                                                         
	        if (start_single_read = '1') then                                          
	          -- Signals a new read address is                                         
	          -- available by user logic                                               
	          read_index <= std_logic_vector (unsigned(read_index) + 1);                                          
	        end if;                                                                    
	      end if;                                                                      
	    end if;                                                                        
	  end process;                                                                                                                                       
	  process(M_AXI_ACLK)                                                              
	  begin                                                                            
	    if (rising_edge (M_AXI_ACLK)) then                                             
	      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                 
	        axi_arvalid <= '0';                                                        
	      else                                                                         
	        if (start_single_read = '1') then                                          
	          --Signal a new read address command is available by user logic           
	          axi_arvalid <= '1';                                                      
	        elsif (M_AXI_ARREADY = '1' and axi_arvalid = '1') then                     
	        --RAddress accepted by interconnect/slave (issue of M_AXI_ARREADY by slave)
	          axi_arvalid <= '0';                                                      
	        end if;                                                                    
	      end if;                                                                      
	    end if;                                                                        
	  end process;                                                                     

	  process(M_AXI_ACLK)                                             
	  begin                                                                 
	    if (rising_edge (M_AXI_ACLK)) then                                  
	      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                      
	        axi_rready <= '1';                                              
	      else                                                              
	        if (M_AXI_RVALID = '1' and axi_rready = '0') then               
	         -- accept/acknowledge rdata/rresp with axi_rready by the master
	         -- when M_AXI_RVALID is asserted by slave                      
	          axi_rready <= '1';                                            
	        elsif (axi_rready = '1') then                                   
	          -- deassert after one clock cycle                             
	          axi_rready <= '0';                                            
	        end if;                                                         
	      end if;                                                           
	    end if;                                                             
	  end process;                                                          
	                  

	    	  

	axi_wdata(7 downto 0) <= reg_pixels;                  
	                                                                        
	--Flag write errors                                                     
	  read_resp_error <= (axi_rready and M_AXI_RVALID and M_AXI_RRESP(1));  

	--  Write Addresses                                                               
	    process(M_AXI_ACLK)                                                                 
	      begin                                                                            
	    	if (rising_edge (M_AXI_ACLK)) then                                              
	    	  if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                 
	    	    axi_awaddr <= (others => '0');                                              
	    	  elsif (M_AXI_AWREADY = '1' and axi_awvalid = '1') then                        
	    	    -- Signals a new write address/ write data is                               
	    	    -- available by user logic                                                  
	    	    axi_awaddr <= std_logic_vector (unsigned(axi_awaddr) + 4);                                     
	    	  end if;                                                                       
	    	end if;                                                                         
	      end process;                                                                     
	                                                                                       
	-- Read Addresses                                                                      
	    process(M_AXI_ACLK)                                                                
	   	  begin                                                                         
	   	    if (rising_edge (M_AXI_ACLK)) then                                          
	   	      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                              
	   	        axi_araddr <= (others => '0');                                          
	   	      elsif (M_AXI_ARREADY = '1' and axi_arvalid = '1') then                    
	   	        -- Signals a new write address/ write data is                           
	   	        -- available by user logic                                              
	   	        axi_araddr <= std_logic_vector (unsigned(axi_araddr) + 4);                                 
	   	      end if;                                                                   
	   	    end if;                                                                     
	   	  end process;                                                                  
		                                                                                    
	-- Write data                                                                          
	    process(M_AXI_ACLK)                                                                
		  begin                                                                             
		    if (rising_edge (M_AXI_ACLK)) then                                              
		      if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                  
		        axi_wdata <= C_M_START_DATA_VALUE;    	                                    
		      elsif (M_AXI_WREADY = '1' and axi_wvalid = '1') then                          
		        -- Signals a new write address/ write data is                               
		        -- available by user logic                                                  
		        axi_wdata <= std_logic_vector (unsigned(C_M_START_DATA_VALUE) + unsigned(write_index));    
		      end if;                                                                       
		    end if;                                                                         
		  end process;                                                                      		                                                                                    
		                                                                                    
	-- Expected read data                                                                  
	    process(M_AXI_ACLK)                                                                
	    begin                                                                              
	      if (rising_edge (M_AXI_ACLK)) then                                               
	        if (M_AXI_ARESETN = '0' or mst_exec_state = WAIT4IRQ) then                                                   
	          expected_rdata <= C_M_START_DATA_VALUE;    	                                
	        elsif (M_AXI_RVALID = '1' and axi_rready = '1') then                           
	          -- Signals a new write address/ write data is                                
	          -- available by user logic                                                   
	          expected_rdata <= std_logic_vector (unsigned(C_M_START_DATA_VALUE) + unsigned(read_index)); 
	        end if;                                                                        
	      end if;                                                                          
	    end process;                                                                       
	  --implement master command interface state machine                                           
	  
	  MASTER_EXECUTION_PROC:process(M_AXI_ACLK)                                                         
           	variable scancode : std_logic_vector(7 downto 0);
           	variable extended, keyup : std_logic;
            variable shift_l_down, ctrl_l_down, alt_l_down : std_logic; 
            variable shift_r_down, ctrl_r_down, alt_r_down : std_logic;
            variable next_state : state;                                                         
	  begin                                                                                             
	    if (rising_edge (M_AXI_ACLK)) then                                                              
	      if (M_AXI_ARESETN = '0' ) then                                                                
	        -- reset condition                                                                          
	        -- All the signals are ed default values under reset condition                              
	        mst_exec_state  <= INITMEM;                                                            
	        start_single_write <= '0';                                                                  
	        write_issued   <= '0';                                                                      
	        start_single_read  <= '0';                                                                  
	        read_issued  <= '0';                                                                        
	        compare_done   <= '0';                                                                      
--	        ERROR <= '0'; 
	      
	      	current_char <= 0;
            current_line <= 0;
            scan_line <= 0;
            reg_pixels <= X"00";
            shift_l_down := '0';
            ctrl_l_down := '0';
            alt_l_down := '0';
            shift_r_down := '0';
            ctrl_r_down := '0';
            alt_r_down := '0';
            keyup := '0';
            extended := '0';
	        
	      else                                                                                          
	        -- state transition                                                                         
	        case (mst_exec_state) is                                                                    
	                                                                                                   
	          when INITMEM =>  
				-- This state is for doing write transactions to                       
                if (M_AXI_BVALID = '1' and axi_bready='1') then                                                             
                    if ( scan_line = CHAR_HEIGHT-1 ) then
                        scan_line <= 0;
                        if ( current_char = CHARS_PER_LINE-1 ) then
                            current_char <= 0;
                            if ( current_line = LINES_PER_PAGE-1 ) then
                                current_line <= 0;
                                mst_exec_state <= WAIT4IRQ;
                            else
                                current_line <= current_line + 1;
                            end if;
                        else
                            current_char <= current_char + 1;
                        end if;
                    else
                        scan_line <= scan_line + 1;
                        mst_exec_state <= INITMEM;
                    end if;
                    write_issued <= '0';
                else                                                                                    
                  mst_exec_state  <= INITMEM;

                  if (axi_awvalid = '0' and axi_wvalid = '0' and M_AXI_BVALID = '0' and                 
                    start_single_write = '0' and write_issued = '0') then          
                    start_single_write <= '1';                                                          
                    write_issued  <= '1';                                                               
                  elsif (axi_bready = '1') then                                                         
                    write_issued   <= '0';                                                              
                  else                                                                                  
                    start_single_write <= '0'; --Negate to generate a pulse                             
                  end if;                                                                               
                end if;
                
                
	          when WAIT4IRQ =>                                                                      
	            -- This state is just a pause between READ/WRITE transactions
                if ( IRQ_I = '1' ) then
    	            mst_exec_state  <= WAIT4CODE;                                                        
                end if;                                                                                 
	                                                                                                   
	          when WAIT4CODE =>                                                                        
	           	            -- This state is responsible to issue start_single_read pulse to                        
	            -- initiate a read transaction. Read transactions will be                               
	            -- issued continuously.                                           
	            -- read controller                                                                      
	            if (M_AXI_RVALID = '1' and axi_rready = '1') then
	                next_state := WAIT4IRQ;         
	                scancode := M_AXI_RDATA(7 downto 0);                                                      
                    if ( scancode = X"F0" ) then
                        keyup := '1';
                    elsif ( scancode = X"E0" ) then
                        extended := '1';
                    else
                        if ( keyup = '1' ) then
                            if ( scancode = X"59" ) then 
                                shift_r_down := '0';
                            elsif ( scancode = X"14" and extended = '1' ) then 
                                ctrl_r_down := '0';
                            elsif ( scancode = X"11" and extended = '1' ) then 
                                alt_r_down := '0';
                            elsif ( scancode = X"12" ) then
                                shift_l_down := '0';
                            elsif ( scancode = X"14" ) then 
                                ctrl_l_down := '0';
                            elsif ( scancode = X"11" ) then 
                                alt_l_down := '0';
                            end if;
                        elsif ( extended = '1' ) then
                            if ( scancode = X"14" ) then
                                ctrl_r_down := '1';
                            elsif ( scancode = X"11" ) then 
                                alt_r_down := '1';
                            end if;
                        elsif ( scancode = X"12" ) then
                            shift_l_down := '1';
                        elsif ( scancode = X"14") then
                            ctrl_l_down := '1';
                        elsif ( scancode = X"11") then
                            alt_l_down := '1';
                        elsif ( scancode = X"59" ) then
                            shift_r_down := '1';
                        else
                            next_state := GETPIXELS1;
                        end if;
    
                        keyup := '0';
                        extended := '0';
                    end if;
    
                    -- transfer variables to signals
                    mst_exec_state <= next_state;
                    shift <= shift_l_down or shift_r_down;
                    ctrl <= ctrl_l_down or ctrl_r_down;
                    alt <= alt_l_down or alt_r_down;
                    code <= scancode;
                  read_issued   <= '0';
	            else                                                                                    
	              mst_exec_state  <= WAIT4CODE;                                                        

	              if (axi_arvalid = '0' and M_AXI_RVALID = '0' and                  
	                start_single_read = '0' and read_issued = '0') then                                 
	                start_single_read <= '1';                                                           
	                read_issued   <= '1';                                                               
	              elsif (axi_rready = '1') then                                                         
	                read_issued   <= '0';                                                               
	              else                                                                                  
	                start_single_read <= '0'; --Negate to generate a pulse                              
	              end if;                                                                               
	            end if;                                                                                 
	                                                                                                    
	          when GETPIXELS1 =>                                                                      
	            -- This state is responsible to issue the state of comparison                           
	            -- of written data with the read data. If no error flags are set,                       
	            -- compare_done signal will be aseted to indicate success.                             
	                mst_exec_state <= GETPIXELS2;
    
            when GETPIXELS2 =>
                -- if the character is a carriage return, go to the next line
                pixelnum <= 0;
                if ( ascii = X"0D" ) then
                    current_char <= 0;
                    if ( current_line = LINES_PER_PAGE-1 ) then
                        current_line <= 0;
                    else
                        current_line <= current_line + 1;
                    end if;
                    mst_exec_state <= WAIT4IRQ;
                -- if the character is a back space, backup a character
                elsif ( ascii = X"08" ) then
                    if ( scan_line = 0 ) then
                        if ( current_char = 0 ) then
                            current_char <= CHARS_PER_LINE-1;
                            if ( current_line = 0 ) then
                                current_line <= 0;
                            else
                            current_line <= current_line - 1;
                            end if;
                        else
                            current_char <= current_char - 1;
                        end if;
                    end if;
                    reg_pixels <= X"00"; -- clear the character
                    mst_exec_state <= WAIT4MEMACK;
                else
                    reg_pixels <= pixels;
                    mst_exec_state <= WAIT4MEMACK;
                end if;
                
            when WAIT4MEMACK =>
            
                if (M_AXI_BVALID = '1' and axi_bready='1') then                                                             
                    write_issued   <= '0';                                                              
                    if ( scan_line = CHAR_HEIGHT-1 ) then
                        mst_exec_state <= WAIT4IRQ;
                        scan_line <= 0;
                        if ( ascii /= X"08" ) then -- increment the cursor if it wasnt a back space
                            if ( current_char = CHARS_PER_LINE-1 ) then
                                current_char <= 0;
                                if ( current_line = LINES_PER_PAGE-1 ) then
                                    current_line <= 0;
                                else
                                    current_line <= current_line + 1;
                                end if;
                            else
                                current_char <= current_char + 1;
                            end if;
                        end if;
                    else
                        scan_line <= scan_line + 1;
                        mst_exec_state <= GETPIXELS1;
                    end if;
                else                                                                                    
                  mst_exec_state  <= WAIT4MEMACK;

                  if (axi_awvalid = '0' and axi_wvalid = '0' and M_AXI_BVALID = '0' and                 
                    start_single_write = '0' and write_issued = '0') then          
                    start_single_write <= '1';                                                          
                    write_issued  <= '1';                                                               
                  elsif (axi_bready = '1') then                                                         
                    write_issued   <= '0';                                                              
                  else                                                                                  
                    start_single_write <= '0'; --Negate to generate a pulse                             
                  end if;                                                                               
                end if;                                                                  
	                                                                                                    
	          when others  =>                                                                           
	              mst_exec_state  <= WAIT4IRQ;                                                      
	        end case;                                                                                 
	      end if;                                                                                       
	    end if;                                                                                         
	  end process;       
	  
	  rst_p <= not M_AXI_ARESETN;

    -- instantiate the 8x12 lookup table here

    Inst_char_lookup : char_lookup_table 
        port map( 
            clk => M_AXI_ACLK,
            reset => rst_p, --M_AXI_ARESETN,
            ascii => ascii, --x"41",
            line => scan_line, --current_line,
            pixels => pixels

        );

        --pixels <= x"7e";


    -- instantiate the scancode to ascii component here

    Inst_scancode_ascii : scancode2ascii 
        port map(
            scancode => code,
            ascii => ascii, --ascii,
            shift => shift,
            ctrl => ctrl,
            alt => alt

        );                                                                         
	                                                                                                    
end implementation;
