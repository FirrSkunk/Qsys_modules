--sACN Module
--Based on Christian Cook's Artnet Module
--Modified version of P.Burnetts AMX sACN Module
--Modified again by Will To allow per channel priority



UNIVERSE_SIZE = 512

sequenceNum = 0

local function changeUniverse(uIn)
  --print(uIn)
  universe = uIn
  uByteHi = (universe >> 8) & 0xff
  uByteLo = universe & 0xff
  destIP = "239.255.".. uByteHi.."."..uByteLo
  --print("239.255."..uByteHi.."."..uByteLo)
  --print(destIP)
  
  --txSock:Close(); txSock:Open();
end

local function updateDMX()
  local packet = {

    --5.1 Preamble Size

    0x00,0x10,

    --5.2 Post-amble Size
    00,00,

    --5.3 ACN Packet Identifier

    0x41,0x53,0x43,0x2d,0x45,0x31,0x2e,0x31,0x37,0x00,0x00,0x00,

    --5.4 Flags & Length

    0x72,0x6e,

    --5.5 Vector

    0x00,0x00,0x00,0x04,
  
    --5.6 CID (Component Identifier)
    ]]
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    --[[
    6 E1.31 Framing Layer Protocol
    6.1 Flags & Length
    The E1.31 Flags & Length field is a 16-bit field with the PDU length encoded in the low 12 bits and 0x7 in
    the top 4 bits.
    7 6 5 4 3 2 1 0
    +-------------------------------+
    | flags=0x7 | length-hi |
    +-------------------------------+
    | length-lo |
    +-------------------------------+
    Figure 2: E1.31 Flags and Length
    The E1.31 framing layer PDU length is computed starting with octet 38 and continuing through the last
    property value provided in the DMP PDU (octet 637 for a full payload). This is the length of the E1.31
    framing layer PDU.
    ]]
    0x72,0x58,
    --[[
    6.2 Vector
    Transmitters shall set the E1.31 Layer's Vector to 0x0000002. Receivers shall discard the packet if the
    received value is not 0x00000002. This value indicates that the E1.31 framing layer is wrapping a DMP
    PDU.
    ]]
    0x00,0x00,0x00,0x02,
    --[[
    6.3 Source Name
    A user assigned name provided by the source of the packet for use in displaying the identity of a source to
    a user. There is no mechanism, other than user configuration of sources, to ensure uniqueness of this
    name. The source name shall be null terminated. If the source component implements ACN discovery as
    defined in EPI 19 [ACN] then this name shall be the same as the UACN field specified in EPI 19 [ACN].
    ]]
    
    --vvvvvv--packet label in hex--vvvvvvv--
    
      --must match string length
    
      0x4c,0x4d,0x47,0x20,0x53,0x59,0x53,0x54,0x45,0x4d,0x53,0x20,0x49,0x4e,0x54,0x45,0x47,0x52,0x41,0x54,0x49,0x4f,0x4e,0x20,0x2d,0x20,0x51,0x2d,0x53,0x59,0x53,0x20,
      0x53,0x41,0x43,0x4e,0x20,0x42,0x52,0x4f,0x41,0x44,0x43,0x41,0x53,0x54,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    
    --^^^^^^--packet label in hex--^^^^^^--
    
    --[[
    6.4 Priority
    A receiver conforming to this standard may receive data for the same universe from multiple sources that
    may be distinguished by examining the CID in the packet. (This is a situation that cannot occur in
    conventional DMX systems.)
    The Priority field is an unsigned one octet field. The value is used by receivers in selecting between
    multiple sources of data for a given universe number. Sources that do not support variable priority shall
    transmit a priority of 100. No priority outside the range of 0 to 200 shall be transmitted on the network.
    Priority increases with numerical value, i.e., 200 is a higher priority than 100.
    For a given universe number, an E1.31 receiver shall treat data from packets with the highest priority as
    the definitive data for that universe.
    6.4.1 Multiple Sources at Highest Priority
    It is possible for there to be multiple sources, all transmitting data at the highest currently active priority
    level for a given universe. When this occurs, receivers must handle these sources in some way.
    A receiver which is only capable of processing one source of data will encounter a sources exceeded
    condition when two or more sources are present.
    ]]
    100,
    --[[
    --Reserved
    Transmitter Shall Send 0
    Receivers Shall Ignore
    ]]
    0x00,0x00,
    --[[
    --Sequence Number
    To detect duplicate or out of
    order packets.
    Receivers that do not support sequence numbering of packets shall ignore the contents of this field.
    Having first received a packet with sequence number A, a second packet with sequence number B
    arrives. If, using signed 8-bit binary arithmetic, B – A is less than or equal to 0, but greater than -20 then
    the packet containing sequence number B shall be deemed out of sequence and discarded.
    ***Note: This algorithm allows the sequence stream from a source to jump
    by large amounts without undue delay, as in the case of a reset, without
    allowing packets received slightly out of order to cause flicker or interfere
    with predictive algorithms found in many moving light fixtures.
    For receivers supporting sequence numbering, packets shall be processed in the order received unless
    they are discarded according to the above algorithm.
    ]]
    (sequenceNum & 0xFF),
    --[[
    --Options Flags
    Bit 7 = Preview_Data
    Bit 6 = Stream_Terminated
    ]]
    0,
    --[[
    --Universe
    Identifier for a distinct
    stream of DMX Data
    ]]
    uByteHi,uByteLo,
    --[[7.1 Flags & Length
    The DMP Layer's Flags & Length field is a 16-bit field with the PDU length encoded in the low 12 bits and
    0x7 in the top 4 bits.
    7 6 5 4 3 2 1 0
    +-------------------------------+
    | flags=0x7 | length-hi |
    +-------------------------------+
    | length-lo |
    +-------------------------------+
    Figure 3: DMP Flags and Length
    The DMP layer PDU length is computed starting with octet 115 and continuing through the last property
    value provided in the DMP PDU (octet 637 for a full payload). This is the length of the DMP PDU.
    ]]
    0x72,0x0b,
    --[[
    7.2 Vector
    The DMP Layer's Vector shall be set to 0x02, which indicates a DMP Set Property message by
    transmitters. Receivers shall discard the packet if the received value is not 0x02.
    ]]
    0x02,
    --[[
    7.3 Address Type and Data Type
    Transmitters shall set the DMP Layer's Address Type and Data Type to 0xa1. Receivers shall discard the
    packet if the received value is not 0xa1.
    Identifies format of address and data
    ]]
    0xa1,
    --[[
    7.4 First Property Address
    Transmitters shall set the DMP Layer's First Property Address to 0x0000. Receivers shall discard the
    packet if the received value is not 0x0000.
    Indicates DMX START Code is at DMP address 0
    ]]
    0x00,0x00,
    --[[
    7.5 Address Increment
    Transmitters shall set the DMP Layer's Address Increment to 0x0001. Receivers shall discard the packet if
    the received value is not 0x0001.
    Indicates each property is 1 octet
    ]]
    0x00,0x01,
    --[[
    7.6 Property Value Count (Number of DMX512-A Data Slots)
    The DMP Layer's Property Value Count is used to encode the number of DMX512-A [DMX] Slots
    (including the START Code slot).
    Indicates 1+ the number of slots in packet,. 0x0001 -- 0x0201
    ]]
    0x02,0x01,
    --[[
    7.7 Property Values (DMX512-A Data)
    The DMP Layer's Property values field is used to encode the DMX512-A [DMX] START Code and data.
    The first octet of the property values field shall be the DMX512-A [DMX] START Code.
    The remainder of the property values shall be the DMX512-A data slots. This data shall contain a
    sequence of octet data values that represent a consecutive group of data slots, starting with slot 1, from a
    DMX512-A packet. The number of data slots encoded in the frame shall not exceed the DMX512-A limit of
    512 data slots.
    Processing of Alternate START Code data shall be compliant with ANSI E1.11 [DMX] Section 8.5.3.3 -
    Handling of Alternate START Code packets by in-line devices.
    ]]
    0x00
    }
  
  
  for i=1,UNIVERSE_SIZE,1 do 
    if (i < 257) then
      val = Controls.DMXChan1[i].Value & 0xff
    else
      val = Controls.DMXChan2[i-256].Value & 0xff
    end
    if(val > 255) then val = 255 end
    if(val < 0) then val = 0 end
    packet[#packet+1] = val
  end
  
  sequenceNum = sequenceNum + 1
  if(sequenceNum > 255) then
    sequenceNum = 0
  end
  
  --print(pack(packet))
  txSock:Send(destIP,5568,pack(packet))
end

local function updateSlotPriorities()
  local packet = {
    --[[	
    5.1 Preamble Size
    Transmitters shall set the Preamble Size to 0x0010. Receivers of UDP [UDP] based E1.31 shall discard
    the packet if the Preamble Size is not 0x0010. The preamble contains the preamble size field, the postamble
    size field, and the ACN packet identifier and has a length of 0x10 octets.
    ]]
    0x00,0x10,
    --[[
    5.2 Post-amble Size
    There is no post-amble for RLP used over UDP [UDP]; therefore the Post-amble Size is 0x0.
    Transmitters shall set the Post-amble Size to 0x0000. Receivers of UDP based E1.31 shall discard the
    packet if the Post-amble Size is not 0x0000.
    ]]
    00,00,
    --[[
    5.3 ACN Packet Identifier
    The ACN Packet Identifier shall contain the following sequence of hexadecimal characters 0x41 0x53 0x43
    0x2d 0x45 0x31 0x2e 0x31 0x37 0x00 0x00 0x00.
    Receivers shall discard the packet if the ACN Packet Identifier is not valid.
    ]]
    0x41,0x53,0x43,0x2d,0x45,0x31,0x2e,0x31,0x37,0x00,0x00,0x00,
    --[[
    5.4 Flags & Length
    The Root Layer's Flags & Length field is a 16-bit field with the PDU length encoded in the low 12 bits and
    0x7 in the top 4 bits.
    7 6 5 4 3 2 1 0
    +-------------------------------+
    | flags=0x7 | length-hi |
    +-------------------------------+
    | length-lo |
    +-------------------------------+
    Figure 1: RLP Flags and Length
    The RLP PDU length is computed starting with octet 16 and counting all octets in the packet through the
    last Property Value provided in the DMP layer (Octet 637 for a full payload). This is the length of the RLP
    PDU.
    ]]
    0x72,0x6e,
    --[[
    5.5 Vector
    Transmitters shall set the Root Layer's Vector to 0x0000004. Receivers shall discard the packet if the
    received value is not 0x00000004. This value indicates that the root layer PDU is wrapping an E1.31
    framing layer PDU.
    ]]
    0x00,0x00,0x00,0x04,
    --[[
    5.6 CID (Component Identifier)
    The Root Layer contains a CID. The CID shall be a UUID (Universally Unique Identifier) [UUID] that is a
    128-bit number that is unique across space and time compliant with RFC 4122 [UUID]. Each piece of
    equipment should maintain the same CID for its entire lifetime (e.g. by storing it in read-only memory).
    This means that a particular component on the network can be identified as the same entity from day to
    day despite network interruptions, power down and so on. However, in some systems there may be
    situations in which volatile components are dynamically created “on the fly” and in these cases the
    controlling process can generate CIDs as required. The choice of UUIDs for CIDs allows them to be
    generated as required without reference to any registration process or authority. As with all E1.31 packet
    contents, the CID shall be transmitted in network byte order (big endian).

    To help insure that we have a unique ID the CID for this system is created using the time and date obtained at startup,.  
    DD/MM/YYHH:MM:SS
    ]]
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    --[[
    6 E1.31 Framing Layer Protocol
    6.1 Flags & Length
    The E1.31 Flags & Length field is a 16-bit field with the PDU length encoded in the low 12 bits and 0x7 in
    the top 4 bits.
    7 6 5 4 3 2 1 0
    +-------------------------------+
    | flags=0x7 | length-hi |
    +-------------------------------+
    | length-lo |
    +-------------------------------+
    Figure 2: E1.31 Flags and Length
    The E1.31 framing layer PDU length is computed starting with octet 38 and continuing through the last
    property value provided in the DMP PDU (octet 637 for a full payload). This is the length of the E1.31
    framing layer PDU.
    ]]
    0x72,0x58,
    --[[
    6.2 Vector
    Transmitters shall set the E1.31 Layer's Vector to 0x0000002. Receivers shall discard the packet if the
    received value is not 0x00000002. This value indicates that the E1.31 framing layer is wrapping a DMP
    PDU.
    ]]
    0x00,0x00,0x00,0x02,
    --[[
    6.3 Source Name
    A user assigned name provided by the source of the packet for use in displaying the identity of a source to
    a user. There is no mechanism, other than user configuration of sources, to ensure uniqueness of this
    name. The source name shall be null terminated. If the source component implements ACN discovery as
    defined in EPI 19 [ACN] then this name shall be the same as the UACN field specified in EPI 19 [ACN].
    ]]
    
    --vvvvvv--packet label in hex--vvvvvvv--
    
      --must match string length
    
      0x4c,0x4d,0x47,0x20,0x53,0x59,0x53,0x54,0x45,0x4d,0x53,0x20,0x49,0x4e,0x54,0x45,0x47,0x52,0x41,0x54,0x49,0x4f,0x4e,0x20,0x2d,0x20,0x51,0x2d,0x53,0x59,0x53,0x20,
      0x53,0x41,0x43,0x4e,0x20,0x42,0x52,0x4f,0x41,0x44,0x43,0x41,0x53,0x54,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    
    --^^^^^^--packet label in hex--^^^^^^--
    
    --[[
    6.4 Priority
    A receiver conforming to this standard may receive data for the same universe from multiple sources that
    may be distinguished by examining the CID in the packet. (This is a situation that cannot occur in
    conventional DMX systems.)
    The Priority field is an unsigned one octet field. The value is used by receivers in selecting between
    multiple sources of data for a given universe number. Sources that do not support variable priority shall
    transmit a priority of 100. No priority outside the range of 0 to 200 shall be transmitted on the network.
    Priority increases with numerical value, i.e., 200 is a higher priority than 100.
    For a given universe number, an E1.31 receiver shall treat data from packets with the highest priority as
    the definitive data for that universe.
    6.4.1 Multiple Sources at Highest Priority
    It is possible for there to be multiple sources, all transmitting data at the highest currently active priority
    level for a given universe. When this occurs, receivers must handle these sources in some way.
    A receiver which is only capable of processing one source of data will encounter a sources exceeded
    condition when two or more sources are present.
    ]]
    100,
    --[[
    --Reserved
    Transmitter Shall Send 0
    Receivers Shall Ignore
    ]]
    0x00,0x00,
    --[[
    --Sequence Number
    To detect duplicate or out of
    order packets.
    Receivers that do not support sequence numbering of packets shall ignore the contents of this field.
    Having first received a packet with sequence number A, a second packet with sequence number B
    arrives. If, using signed 8-bit binary arithmetic, B – A is less than or equal to 0, but greater than -20 then
    the packet containing sequence number B shall be deemed out of sequence and discarded.
    ***Note: This algorithm allows the sequence stream from a source to jump
    by large amounts without undue delay, as in the case of a reset, without
    allowing packets received slightly out of order to cause flicker or interfere
    with predictive algorithms found in many moving light fixtures.
    For receivers supporting sequence numbering, packets shall be processed in the order received unless
    they are discarded according to the above algorithm.
    ]]
    (sequenceNum & 0xFF),
    --[[
    --Options Flags
    Bit 7 = Preview_Data
    Bit 6 = Stream_Terminated
    ]]
    0,
    --[[
    --Universe
    Identifier for a distinct
    stream of DMX Data
    ]]
    uByteHi,uByteLo,
    --[[7.1 Flags & Length
    The DMP Layer's Flags & Length field is a 16-bit field with the PDU length encoded in the low 12 bits and
    0x7 in the top 4 bits.
    7 6 5 4 3 2 1 0
    +-------------------------------+
    | flags=0x7 | length-hi |
    +-------------------------------+
    | length-lo |
    +-------------------------------+
    Figure 3: DMP Flags and Length
    The DMP layer PDU length is computed starting with octet 115 and continuing through the last property
    value provided in the DMP PDU (octet 637 for a full payload). This is the length of the DMP PDU.
    ]]
    0x72,0x0b,
    --[[
    7.2 Vector
    The DMP Layer's Vector shall be set to 0x02, which indicates a DMP Set Property message by
    transmitters. Receivers shall discard the packet if the received value is not 0x02.
    ]]
    0x02,
    --[[
    7.3 Address Type and Data Type
    Transmitters shall set the DMP Layer's Address Type and Data Type to 0xa1. Receivers shall discard the
    packet if the received value is not 0xa1.
    Identifies format of address and data
    ]]
    0xa1,
    --[[
    7.4 First Property Address
    Transmitters shall set the DMP Layer's First Property Address to 0x0000. Receivers shall discard the
    packet if the received value is not 0x0000.
    Indicates DMX START Code is at DMP address 0
    ]]
    0x00,0x00,
    --[[
    7.5 Address Increment
    Transmitters shall set the DMP Layer's Address Increment to 0x0001. Receivers shall discard the packet if
    the received value is not 0x0001.
    Indicates each property is 1 octet
    ]]
    0x00,0x01,
    --[[
    7.6 Property Value Count (Number of DMX512-A Data Slots)
    The DMP Layer's Property Value Count is used to encode the number of DMX512-A [DMX] Slots
    (including the START Code slot).
    Indicates 1+ the number of slots in packet,. 0x0001 -- 0x0201
    ]]
    0x02,0x01,
    --[[
    7.7 Property Values (DMX512-A Data)
    The DMP Layer's Property values field is used to encode the DMX512-A [DMX] START Code and data.
    The first octet of the property values field shall be the DMX512-A [DMX] START Code.
    The remainder of the property values shall be the DMX512-A data slots. This data shall contain a
    sequence of octet data values that represent a consecutive group of data slots, starting with slot 1, from a
    DMX512-A packet. The number of data slots encoded in the frame shall not exceed the DMX512-A limit of
    512 data slots.
    Processing of Alternate START Code data shall be compliant with ANSI E1.11 [DMX] Section 8.5.3.3 -
    Handling of Alternate START Code packets by in-line devices.
    ]]
    0xdd
    }
  
  
  for i=1,UNIVERSE_SIZE,1 do 
    if (i < 257) then
      val = Controls.SlotPriorities1[i].Value & 0xff
    else
      val = Controls.SlotPriorities2[i-256].Value & 0xff
    end
    if(val > 255) then val = 255 end
    if(val < 0) then val = 0 end
    packet[#packet+1] = val
  end
  
  sequenceNum = sequenceNum + 1
  if(sequenceNum > 255) then
    sequenceNum = 0
  end
  
  --print(pack(packet))
  txSock:Send(destIP,5568,pack(packet))
end

function pack(t)
  local s = "";
  for k,v in pairs(t) do
    s = s .. string.char(v);
  end;
  return s;
end;


DmxTimer = Timer.New();
DmxTimer.EventHandler = function()
  if(Controls.UseSlotPriorities.Value == 1) then
    updateSlotPriorities()
  end
  updateDMX()
end
DmxTimer:Start(0.03);

txSock = UdpSocket.New()
txSock:Open()

SocketTimer = Timer.New();
SocketTimer.EventHandler = function()
  txSock:Close(); txSock:Open();
end;
SocketTimer:Start(5);

changeUniverse(Controls.Universe.String)

Controls.Universe.EventHandler = function()
  changeUniverse(Controls.Universe.String)
end

