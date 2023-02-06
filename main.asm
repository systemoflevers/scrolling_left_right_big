INCLUDE "hardware.inc"

SECTION "Header", ROM0[$100]

EntryPoint:
        di ; Disable interrupts. That way we can avoid dealing with them, especially since we didn't talk about them yet :p
        jp Start

REPT $150 - $104
    db 0
ENDR

SECTION "Game code", ROM0

Start:

;; Busy loop to poll until VBlank is reached.
.waitVBlank
  ld a, [rLY]
  cp 144 ; Check if the LCD is past VBlank (vblank starts at line 144)
  jr c, .waitVBlank ; c: carry flag means 144 > [rLY], so we haven't reached 144, so loop.

  ;; Turn off the display.
  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.

  ;; Load the tile data from ROM to VRAM at address $8000
  ld bc, TileSet.end - TileSet
  ld hl, _VRAM8000
  ld de, TileSet
  call memcpy_big

  ;; Initialize the ROM bank to $001 and WRAM variables to keep track of it.
  ld a, 0
  ;;ld a, $1 ; for testing
  ld [vCurrentBankHigh], a
  ld [rROMB1], a
  ld a, $01
  ;;ld a, $FF ; for testing
  ld [vCurrentBankLow], a
  ld [rROMB0], a

  ;; Load the first chuck of the tile map from ROM to the _SCRN0 VRAM tile map.
  ld c, 0        ; counter for number of columns copied
  ld hl, TileMap ; source address of tile map data
  ;;ld HL, $7E5E ; for testing
  ld de, _SCRN0  ; destination address of tile map data
.tile_map_copy_loop
  call copy_column
  inc c
  ld a, 21
  cp c
  jr z, .finish_setup ;; copied all the columns, exit the loop
  ;; Since we're copying by column, the start address for the destination of
  ;; the Cth column is _SCRN0 + C. _SCRN0 is $9800 so load $98 into D and
  ;; C into E for DE to have the correct address.
  ld d, HIGH(_SCRN0)
  ld e, c
  jr .tile_map_copy_loop

.finish_setup
  ;; Store the next column's starting source address from HL to RAM at
  ;; vNextColumnAddress.
  ld d, h
  ld e, l
  ld hl, vNextColumnAddress
  ld a, d
  ld [hli], a
  ld [hl], e
  
  ;; Set 1 column to the left of the screen to the last column in ROM.
  ;; Note, this only works if we started at bank 1 address $4000.
  ld a, 1
  ld [rROMB1], a
  ld a, $ff
  ld [rROMB0], a
  ld hl, $7FEA
  ld de, _SCRN0 + 31
  call copy_column
  ld a, 0
  ;; Set the ROM bank back to 1.
  ld [rROMB1], a
  inc a
  ld [rROMB0], a

  ;; Set BG palette.
  ld hl, $FF47
  ld [hl], %11100100


  ;; Disable interupts but set the VBlank IE flag so that I 
  ;; can use halt to wait for VBlank.
  di
  ld a, %00000001
  ld [rIE], a



  ;; Initialize variable to keep track of the scX value when we last copied a
  ;; column. This is to make sure we don't repeatedly run the copy code if the
  ;; screen is kept at a position that triggered a copy.
  xor a
  ld [vLastSCXCopy], a

  ;; turn the screen back on with background enabled
  ld a, %10010001
  ld [rLCDC], a

.mainLoop

.checkDPad
.checkRight
  ld a, P1F_GET_DPAD
  ld [rP1], a
  ld a, [rP1]
  ld hl, rSCX
  bit 0, a
  jp nz, .checkLeft ; Right wasn't pressed so check left.
  ;; Right was pressed
  ld a, [hl]
  inc a
  ld [vNextSCX], a
  ld b, a
  ld a, [vLastSCXCopy]
  cp b
  jp z, .updateScrollWithHalt ; Nothing to copy, just update SCX.
  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, b
  and %00000111
  jp nz, .updateScrollWithHalt ; not at a tile boundary, no need to copy.

  ;; Need to copy.


  ;; We're going to be copying another column so update vLastSCXCopy now while
  ;; we have the current scX in B.
  ld a, b
  ld [vLastSCXCopy], a
  ;; scX refers to the left edge of the screen but we care about the right so
  ;; add screen width in tiles * tile width in pixels (20 * 8) to get the x 
  add 20 * 8
  ld [vDestinatonSCX], a

  ld a, [vNextColumnAddress]
  ld [vCopySource], a
  ld a, [vNextColumnAddress + 1]
  ld [vCopySource + 1], a
  ld a, [vCurrentBank]
  ld [vSourceBank], a
  ld a, [vCurrentBank + 1]
  ld [vSourceBank + 1], a
  ld hl, vNextColumnAddress
  ld bc, vCurrentBank
  ld de, 18
  call update_source
  jp .doVRAMStuff

.checkLeft
  bit 1, a
  jp nz, .noButtons
  ;; Left was pressed
  ld a, [hl]
  dec a
  ld [vNextSCX], a
  ld b, a
  ld a, [vLastSCXCopy]
  cp b
  jp z, .updateScrollWithHalt ; Nothing to copy, just update SCX
  ;; Now check if we're at a tile boundary. We can know this when scx ends in
  ;; 000
  ld a, b
  and %00000111
  jp nz, .updateScrollWithHalt ; not at a tile boundary, no need to copy.

  ;; Need to do a copy.


  ;; We're going to be copying another column so update vLastSCXCopy now while
  ;; we have the current scX in B.
  ld a, b
  ld [vLastSCXCopy], a
  ;; scX refers to the left edge of the screen and it's currently at the edge of
  ;; a tile. We want to copy tile map values 1 tile column to the left so
  ;; 8 (tile width in pixels).
  sub 8
  ld [vDestinatonSCX], a

  ;; Setup the copy source to be [vNextColumnAddress] - (23 * 18).
  ;; 23 because of the buffer columns we keep on either side of the screen.
  ld de, -(23 * 18)
  ld hl, vCopySource
  ld bc, vSourceBank
  call update_source

  ;; Decrement [vNextColumnAddress] and [vCurrentBank] if needed.
  ld de, -18
  ld hl, vNextColumnAddress
  ld bc, vCurrentBank
  call update_source
  ;;jp .doVRAMStuff

.doVRAMStuff
  ;; Load stuff in preparation of VBlank.
  ld a, [vSourceBank]
  ld [rROMB1], a
  ld a, [vSourceBank + 1]
  ld [rROMB0], a
  ld a, [vDestinatonSCX]
  ;; We need the tile map column. For a give X value that's floor(X / 8). That's
  ;; the same as shifting right 3 times (using logical shift)
  srl a
  srl a
  srl a
  ;; The destination address for the column copy is _SCRN0 + A (A has the tile
  ;; map column we're copying to). Since _SCRN0 is $9800, we set D to $98 and E
  ;; to A.
  ld e, a
  ld d, HIGH(_SCRN0)
  ;; DE now has the destination address.
  ld a, [vCopySource]
  ld h, a
  ld a, [vCopySource + 1]
  ld l, a

  halt ; Wait for VBlank.
  call copy_column
  jp .updateScrollNoHalt


.noButtons
  ;; Nothing to do, halt until next VBlank instead of doing a busy wait for a
  ;; button press.
  halt
  ;; clear rIF so that the next halt will wait for a new VBlank. Otherwise halt
  ;; doesn't do anything while rIF and rIE are set.
  xor a
  ld [rIF], a
  jp .mainLoop

.updateScrollWithHalt
  halt
  ;; clear rIF so that the next halt will wait for a new VBlank. Otherwise halt
  ;; doesn't do anything while rIF and rIE are set.
  xor a
  ld [rIF], a
.updateScrollNoHalt
  ld a, [vNextSCX]
  ld [rSCX], a
  jp .mainLoop


copy_column::
  ;; Copy a column of 18 tile indexes to a destination tile map column.
  ;; The source tile indexes must be stored sequencially.
  ;; Args:
  ;;   de: destination
  ;;   hl: source
  ;; Modifies:
  ;;   a, b, hl, de

  ;; Copying 18 tiles per column.
  ld b, 18
.copy_loop
  ld a, [hli]
  ld [de], a
  dec b
  ret z ;; b is 0, done copy

  ;; The tile map is stored rows, so the next position to copy to is 32 bytes
  ;; from the last position.
  ld a, 32
  add e
  ld e, a
  jr nc, .copy_loop
  inc d
  jr .copy_loop

update_source::
  ;; Updates column ROM address (HL) and the bank (BC) based on DE and the
  ;; vNextColumnAddress and vCurrentBank.
  ;; [HL] = FIX([vNextColumnAddress] + DE)
  ;; Where FIX makes sure it's in the correct $4000-$7FEA range.
  ;; [BC] = [vCurrentBank] +/- 1 needed.
  ;;
  ;; Args:
  ;; hl: where to store the source address
  ;; bc: where to store the ROM bank
  ;; de: how much to change [vNextColumnAddress] by
  ;;     It's absolute value shouldn't be greater than $0FFF. This is to make
  ;;     easier to tell if we went below or above the range.
  push hl
  ld a, [vNextColumnAddress]
  ld h, a
  ld a, [vNextColumnAddress + 1]
  ld l, a
  add hl, de
  ;; Check if it went out of the $4000-$7FEA range.
  ;; If it's below that range then we also decrement the ROM bank.
  ;; If it's above then we increment the ROM bank.

  ;; Check for overflow.
  ;; For there to be an overflow either H > $7F or 
  ;; H == $7F and L > $EA
  ld a, $7F
  cp h
  jp c, .addOverflow
  jp nz, .checkUnderflow
  ld a, $EA
  cp l
  jp nc, .checkUnderflow
.addOverflow
  ;; Add 4 to HL because the tile map values only go up to $7FFC so we need to
  ;; make up for the unused 4 bytes when wrapping around.
  ld a, 4
  add l
  ld l, a
  ld a, 0
  adc h
  ;; Set the upper 4 bits of the source address to $4.
  ;; Tile map values start at address $4000. Because of the assumption that BC
  ;; is <= $0FFF, the wrapped source address after addition can't be > $4FFE, so
  ;; set that 4. 
  and %00001111 ; set the 4 high bits to 0
  set 6, a
  ld h, a
  ;; store the source address

  ;; update the ROM bank.
  ld d, h
  ld e, l
  ld h, b
  ld l, c
  ;; DE has the source address to store.
  ;; HL has where to store the source ROM bank #.
  ;; BC is free
  ld a, [vCurrentBank]
  ld b, a
  ld a, [vCurrentBank + 1]
  ld c, a
  inc bc 
  ;; Check if BC went over $1FF by checking if B is 2.
  ld a, 2
  cp b
  jp nz, .storeStuff
  ;; BC went over, so reset it to $0001. (bank 0 doesn't have tile map values)
  ld b, 0
  ld c, 1
  jp .storeStuff

.checkUnderflow
  ;; HL has the new source address to store.
  ;; BC has where to store the source ROM bank #.
  ;; DE is free.

  ;; We know there was underflow if HL < $4000. Because we subtracted at most
  ;; $0FFF, HL can't be less than $3001. So we just need to check if the high 4
  ;; bits of H are 3 to know if there was underflow.
  ld a, h
  and %11110000
  cp $30
  jp nz, .noUnderflow

  ;; There was underflow.
  ;; set the value correctly
  
  ;; Set the highest 4 bits of HL to $7. Those bits were $3, so we just need
  ;; to set bit 6 to 1.
  set 6, h
  ;; Subtract 4 from HL to make up for the 4 unused bytes at the end of each ROM
  ;; bank.
  ld de, -4
  add hl, de

  ;; Shift around some registers so things are setup for .storeStuff
  ld d, h
  ld e, l
  ld h, b
  ld l, c

  ;; dec the bank
  ld a, [vCurrentBank]
  ld b, a
  ld a, [vCurrentBank + 1]
  ld c, a
  dec bc
  ;; Make sure BC isn't $0000 since bank 0 isn't used for tile map values.
  xor a
  cp b
  jp nz, .storeStuff
  cp c
  jp nz, .storeStuff
  ld bc, $01FF
  jp .storeStuff

.noUnderflow
  ;; HL has the new source address to store.
  ;; BC has where to store the source ROM bank #.
  ;; DE is free.
  ld d, h
  ld e, l
  ld h, b
  ld l, c

  ld a, [vCurrentBank]
  ld b, a
  ld a, [vCurrentBank + 1]
  ld c, a

.storeStuff
  ;; DE has the source address to store.
  ;; HL has where to store the source ROM bank #.
  ;; BC has the new ROM bank#.
  ld a, b
  ld [hli], a
  ld a, c
  ld [hl], a

  pop hl ; where to store the source address
  ld a, d
  ld [hli], a
  ld a, e
  ld [hl], a
  ret
  

SECTION "Tile Set", ROM0
TileSet:
  incbin "numbers.gbgfx"
.end


;; LOOP include map data
FOR N, 0, $1FF
  SECTION "Tile Map {N}", ROMX,BANK[N + 1]

  IF N == 0
    TileMap:
  ENDC

  TileMap\@:
    incbin "long_map.tilemap", N * 16380, 16380
  .end
ENDR
;; END LOOP include map data
TileMapEnd:

SECTION "Work RAM", wram0
vNextColumnAddress:
  ;; Where to read the next column of tile indexes from.
  ds 2
vLastSCXCopy:
  ;; The value of scX the last time a column was copied. This is to know if the
  ;; column for the current position has already been copied.
  ds 1
vCurrentBank:
  ;; Current ROM bank. Split into High and Low variables
vCurrentBankHigh:
  ;; High byte of the current ROM bank. Using an MBC5 so this will be 0 or 1.
  ds 1
vCurrentBankLow:
  ;; Low byte of the current ROM bank.
  ds 1
vNextSCX:
  ;; Used to keep track of scrolling to sync with VBlank
  ds 1
vCopySource:
  ;; Where to get the next 18 tile map values from.
  ds 2
vSourceBank:
  ;; The ROM bank to get the next tile map values from
  ds 2
vDestinatonSCX:
  ;; The SCX of the column to copy to
  ds 1