SECTION "Sachen", HRAM [$FFB0]
SachenMagic1::
	ds 1

SECTION "rst 00", ROM0 [$00]
	nop
SECTION "rst 08", ROM0 [$08]
	nop
SECTION "rst 10", ROM0 [$10]
	nop
SECTION "rst 18", ROM0 [$18]
	nop
SECTION "rst 20", ROM0 [$20]
	nop
SECTION "rst 28", ROM0 [$28]
	nop
SECTION "rst 30", ROM0 [$30]
	nop
SECTION "rst 38", ROM0 [$38]
	di

SECTION "vblank", ROM0 [$40]
	di
	jp VBlank

SECTION "hblank", ROM0 [$48]
	di
	reti

SECTION "timer",  ROM0 [$50]
	di
	call $1492
	reti

SECTION "serial", ROM0 [$58]
	di
	reti

SECTION "joypad", ROM0 [$60]
	di
	reti

SECTION "Home", ROM0

VBlank::
	push af
	call $FF88
	ld a, $01
	ld [$C011], a
	pop af
	ei
	reti

Copy1bpp::
	ld a, [de]
	ldi [hl], a
	ldi [hl], a
	dec bc
	inc de
	ld a, c
	or a, b
	ret z
	jr Copy1bpp

FillVRAM::
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	ld a, [$FF00 + $86]
	ldi [hl], a
	dec bc
	ld a, c
	or a, b
	jr nz, FillVRAM
	ret

SECTION "Entry", ROM0 [$100]
	nop
	jp Main

SECTION "Sachen_Header", ROM0 [$104]
	; Sachen Logo
	db $7C,$E7,$C0,$00,$17,$C8,$CC,$DD,$7C,$88,$8C,$60,$CC,$CC,$66,$66
	db $37,$EE,$EE,$00,$8C,$EF,$66,$66,$31,$0F,$9D,$D9,$F8,$88,$DD,$CC
	db $88,$C7,$06,$C8,$FC,$CC,$E6,$66,$FE,$EF,$E0,$0E,$DC,$CC,$EE,$62
	; Title
	db "               "
	; CGB compat
	db $80
	; New Licensee
	db $00,$00
	; SGB flag
	db $00
	; Cart type
	db $00
	; ROM size
	db $00
	; RAM size
	db $00
	; destination
	db $00 ; Japan
	; Old Licensee
	db $00
	; Mark ROM Version
	db $00 ; Rev 0
	; Header Checksum
	db $87
	; Global Checksum
	db $08,$91

ClearVRAM::
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	xor a, a
	ldi [hl], a
	dec bc
	ld a, c
	or a, b
	ret z
	jr ClearVRAM

SetVRAM::
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	cpl
	ldi [hl], a
	dec bc
	ld a, c
	or a, b
	ret z
	jr SetVRAM

Copy::
	ld a, [de]
	ldi [hl], a
	inc de
	dec bc
	ld a, c
	or a, b
	ret z
	jr Copy

CopyOAM::
	ld a, $C1
	ld [$FF00 + $46], a
	ld a, $28
.cpy_oam_dly:
	dec a
	ret z
	jr .cpy_oam_dly
CopyOAM_End:

SECTION "Fake_Entry", ROM0 [$180]
	nop
	jp $0150
	
SECTION "Nintendo_Header", ROM0 [$184]
	; Nintendo Logo
	db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
	db $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
	db $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
	; Title
	db "               "
	; CGB compat
	db $80
	; New Licensee
	db $00,$00
	; SGB flag
	db $00
	; Cart type
	db $00
	; ROM size
	db $00
	; RAM size
	db $00
	; destination
	db $00 ; Japan
	; Old Licensee
	db $00
	; Mark ROM Version
	db $00 ; Rev 0
	; Header Checksum
	db $87
	; Global Checksum
	db $08,$91

	db $3E ; stray ld a

SECTION "Main", ROM0 [$200]

Main::
	di
	ld [$FF00 + $80], a
	ld a, [$FF00 + $40]
	and a, $80
	jr z, .screen_off
	ld a, $97
	ld [$FF00 + $40], a
.screen_off:
	ld a, [$FF00 + $44]
	cp a, $90
	jr nz, .screen_off
	xor a, a
	ld [$FF00 + $40], a
	ld [$FF00 + $81], a
	ld [$FF00 + $82], a
	ld [$FF00 + $0F], a
	ld [$FF00 + $FF], a
	ld [$FF00 + $85], a
	ld [$FF00 + $26], a
	ld sp, $FFA0
	;; Clear WRAM
	ld hl, $C000
	ld bc, $2000
	call ClearVRAM
	;; Clear BG VRAM
	ld hl, $9800
	ld bc, $07FF
	call SetVRAM
	;; Copy numbers
	ld de, Numbers1bpp
	ld hl, $8000
	ld bc, Numbers1bpp_End - Numbers1bpp
	call Copy1bpp
	;; Copy rest of font
	ld de, Numbers1bpp
	ld hl, $8300
	ld bc, Numbers1bpp_End - Numbers1bpp
	call Copy1bpp
	ld hl, $8410
	ld bc, Font1bpp_End - Font1bpp + $30
	call Copy1bpp
	;; Copy OAM RTN
	ld de, CopyOAM
	ld bc, CopyOAM_End - CopyOAM
	ld hl, $FF88
	call Copy
	;; Copy Intro Gfx
	ld de, SachenScrollGfx
	ld hl, $8900
	ld bc, SachenScrollGfx_End - SachenScrollGfx
	call Copy
	;; 
	ld a, $10
	ld [$FF00 + $41], a
	ld a, [$FF00 + $80]
	cp a, $11
	jr nz, .init_not_cgb
	ld a, $86
	ld [$FF00 + $6A], a
	ld a, $FF
	ld [$FF00 + $6B], a
	ld a, $7F
	ld [$FF00 + $6B], a
	ld a, $80
	ld [$FF00 + $68], a
	ld hl, ColorTable
	ld c, 3 * $08 ; 3 Pals
.copy_cgb_pals:
	ldi a, [hl]
	ld [$FF00 + $69], a
	dec c
	jr nz, .copy_cgb_pals
.init_not_cgb:
	ld sp, $DEFF
	ld a, $FF
	ld [$C709], a
	ld [$C70A], a
	ld [$C70B], a
	ld [$C70C], a
	ld a, $00
	ld [$C706], a
	ld a, $F0
	ld [$FF00 + $26], a
	ld a, $FF
	ld [$FF00 + $24], a
	ld a, $FF
	ld [$FF00 + $25], a
	ld a, $01
	ld [$FF00 + $0F], a
	ld [$FF00 + $FF], a
	ei
	ld a, $97
	ld [$FF00 + $40], a
	call ShowIntro
	ld a, $93
	ld [$FF00 + $40], a
.Mainloop:
	halt
	call MenuBrowse
	call ReadKeypad
	ld a, [$C00F]
	ld [$C010], a
	xor a, a
	ld [$C011], a
	ld a, [$C701]
	ld b, a
	ld a, [$C702]
	ld c, a
	ld a, [$C703]
	and a, b
	and a, c
	cp a, $FF
	jp z, StartGame
	jr .Mainloop

MenuBrowseDelay::
	ld a, [$FF00 + $85]
	inc a
	ld [$FF00 + $85], a
	cp a, $05
	ret nz
	ld a, b
	jr MenuBrowseDelayed
MenuBrowse::
	ld a, [$C013]
	and a, a
	jr nz, DisplayMenuPage
	ld a, [$C00F]
	and a, a
	ret z
	ld b, a
	ld a, [$C010]
	cp a, b
	jr z, MenuBrowseDelay
MenuBrowseDelayed:
	xor a, a
	ld [$FF00 + $85], a
	ld a, b
	bit 2, a
	call nz, MenuMoveUp
	ld a, b
	bit 3, a
	call nz, MenuMoveDown
	ld a, b
	bit 4, a
	call nz, MenuStartGame
	ld a, b
	bit 5, a
	call nz, MenuStartGame
	;; Clear Game Title
	ld hl, $8600
	ld bc, $0180
.copy_to_vram:
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	ldi [hl], a
	dec bc
	ld a, c
	or a, b
	jr nz, .copy_to_vram
	ld d, $00
	ld a, [$C003]
	ld e, a
	ld hl, CursorYPosTbl
	add hl, de
	ldi a, [hl]
	ld [$C100], a
	ld a, [$C00F]
	ld [$C010], a
	call DisplayGameName
	xor a, a
	ld [$FF00 + $83], a
	ld a, $86
	ld [$FF00 + $84], a
	ret
DisplayMenuPage::
	ld a, [$FF00 + $81]
	rla
	ld e, a
	xor a, a
	adc a, a
	ld d, a
	ld hl, PagePtrTbl
	add hl, de
	ldi a, [hl]
	ld e, a
	ldi a, [hl]
	ld d, a
.wait_lyc_vblank:
	ld a, [$FF00 + $44]
	cp a, $90
	jr nz, .wait_lyc_vblank
	call CopyBGData
	xor a, a
	ld [$C013], a
	ret
MenuStartGame:
	ld a, [$C01D]
	and a, a
	ret nz
	inc a
	ld [$C01D], a
	ld a, $01
	ld [$C706], a
	call PlayMusic
	ret
MenuMoveUp::
	ld a, [$FF00 + $82]
	and a, a
	ret z
	dec a
	ld [$FF00 + $82], a
	ld a, [$C003]
	and a, a
	jr z, .MenuMoveUpSwitchPage
	dec a
	ld [$C003], a
	ret
.MenuMoveUpSwitchPage:
	ld a, [$FF00 + $81]
	and a, a
	ret z
	dec a
	ld [$FF00 + $81], a
	ld a, $0E
	ld [$C003], a
	xor a, a
	jr MenuRefreshPage
MenuMoveDown::
	ld a, [$FF00 + $82]
	cp a, $0F
	ret z
	inc a
	ld [$FF00 + $82], a
	ld a, [$C003]
	cp a, $0E
	jr z, .MenuMoveDownSwitchPage
	inc a
	ld [$C003], a
	ret
.MenuMoveDownSwitchPage:
	ld a, [$FF00 + $81]
	cp a, $02
	ret z
	inc a
	ld [$FF00 + $81], a
	xor a, a
	ld [$C003], a
MenuRefreshPage:
	inc a
	ld [$C013], a
	ret
RTN03CC::
	ret
RTN03CD::
	ret
DisplayGameName::
	ld de, $0000
	ld a, [$FF00 + $82]
	rla
	ld e, a
	xor a, a
	adc a, a
	ld d, a
	ld hl, GameNamePtrTbl
	add hl, de
	ldi a, [hl]
	ld e, a
	ldi a, [hl]
	ld d, a
.procString:
	ld a, [de]
	inc de
	cp a, $FF
	ret z
	ld bc, $0020
	ld hl, Chinese1bpp
	inc a
.charMul:
	dec a
	jr z, .charMulDone
	add hl, bc
	jr .charMul
.charMulDone:
	push de
	ld a, [$FF00 + $83]
	ld e, a
	ld a, [$FF00 + $84]
	ld d, a
	ld bc, $0020
.copy_to_vram:
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	ldi a, [hl]
	ld [de], a
	inc de
	ld [de], a
	inc de
	dec bc
	ld a, c
	or a, b
	jr nz, .copy_to_vram
	ld a, e
	ld [$FF00 + $83], a
	ld a, d
	ld [$FF00 + $84], a
	pop de
	jr .procString
	
CopyBGData::
	ld a, [de]
	ld h, a
	inc de
	ld a, [de]
	ld l, a
	inc de
.copy_to_vram:
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	ld a, [de]
	inc de
	and a, a
	jr z, CopyBGData
	cp a, $24
	ret z
	ldi [hl], a
	jr .copy_to_vram

ReadKeypad::
	ld a, $10
	ld [$FF00 + $00], a
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	and a, $0F
	swap a
	ld b, a
	ld a, $20
	ld [$FF00 + $00], a
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	ld a, [$FF00 + $00]
	and a, $0F
	or a, b
	cpl
	ld [$C00F], a
	ld a, $30
	ld [$FF00 + $00], a
	ret

StartGame::
	xor a, a
	ld [$FF00 + $0F], a
	ld [$FF00 + $FF], a
	ld a, [$FF00 + $80]
	cp a, $11
	jr nz, .not_cgb
	call ClearBG_OBJ_Colors
	ld a, $01
	ld [$FF00 + $4F], a
	ld hl, $9A00
	ld bc, $0040
	call ClearVRAM
	xor a, a
	ld [$FF00 + $4F], a
	ld hl, $8000
	ld bc, $0FFF
	call ClearVRAM
.not_cgb:
	ld a, [$FF00 + $82]
	ld b, a
	ld hl, GameEntryBases
	ld e, a
	ld d, $00
	add hl, de
	ldi a, [hl]
	ld [$FF00 + $83], a
	ld e, b
	ld hl, GameEntryMasks
	add hl, de
	ldi a, [hl]
	ld [$FF00 + $84], a
	ld hl, GameEntrySpecVals
	add hl, de
	ldi a, [hl]
	ld [$DFFF], a
	ld [SachenMagic1], a
	ld hl, GameEntryPalPtrTbl
	ld a, b
	rla
	ld e, a
	xor a, a
	adc a, a
	ld d, a
	add hl, de
	ldi a, [hl]
	ld e, a
	ldi a, [hl]
	ld d, a
	or a, d
	and a, a
	jr z, .skip_cust_pal
	ld hl, $0000
	add hl, de
	call CopyBG_OBJ_Colors
.skip_cust_pal:
	ld de, SwitchRTN
	ld hl, $C300
	ld bc, $0100
	call Copy
	ld a, [$FF00 + $82]
	ld sp, GameEntryPoints
	and a, a
.loop_entry_pts:
	jr z, .entry_pt_found
	pop hl
	dec a
	jr .loop_entry_pts
.entry_pt_found:
	jp $C300

LOC04D2::
Numbers1bpp::
INCBIN "gfx/numbers.1bpp"
Numbers1bpp_End:
LOC0522::
Font1bpp::
INCBIN "gfx/font.1bpp"
Font1bpp_End:

CursorYPosTbl::
	db $18,$20,$28,$30,$38,$40,$48,$50,$58,$60,$68,$70,$78,$80,$88

LOC0631::
	db $15,$15,$06
LOC0634::
PagePtrTbl::
	dw MenuHeaderFooter_PageOne
	dw MenuPageOne
	dw MenuPageTwo
	dw SachenScroll_Logo
	dw SachenScroll_Super
	dw SachenScroll_16in1

SachenScroll_Logo::
	db $98,$C4
	db $96,$98,$9A,$9C,$9E,$A0,$A2,$A4,$A6,$A8,$AA,$AC,$AE ; Sachen Logo Line 1
	db $00
	db $98,$E4
	db $97,$99,$9B,$9D,$9F,$A1,$A3,$A5,$A7,$A9,$AB,$AD ; Sachen Logo Line 2
	db $00
	db $99,$27
	db $92,$93,$90,$94,$90,$91,$95 ; PRESENT
	db $24

SachenScroll_Super::
	db $9B,$46
	db $B0,$B4,$B8,$BC,$C0,$C4,$C8,$CC,$D0
	db $00
	db $9B,$66
	db $B1,$B5,$B9,$BD,$C1,$C5,$C9,$CD,$D1
	db $00
	db $9B,$86
	db $B2,$B6,$BA,$BE,$C2,$C6,$CA,$CE,$D2
	db $00
	db $9B,$A6
	db $B3,$B7,$BB,$BF,$C3,$C7,$CB,$CF,$D3
	db $24

SachenScroll_16in1::
	db $98,$06
	db $D4,$D8,$DC,$E0,$E4,$E8,$EC,$F0,$F4
	db $00
	db $98,$26
	db $D5,$D9,$DD,$E1,$E5,$E9,$ED,$F1,$F5
	db $00
	db $98,$46
	db $D6,$DA,$DE,$E2,$E6,$EA,$EE,$F2,$F6
	db $00
	db $98,$66
	db $D7,$DB,$DF,$E3,$E7,$EB,$EF,$F3,$F7
	db $24

MenuHeaderFooter_PageOne::
	db $98,$02
	db " SACHEN^ 16 IN 1",$00 ; Header
	db $9A,$01
	db $60,$62,$20,$64,$66,$20,$68,$6A,$20,$6C,$6E,$20,$70,$72,$20,$74,$76 ; Footer Line 1
	db $00
	db $9A,$21
	db $61,$63,$20,$65,$67,$20,$69,$6B,$20,$6D,$6F,$20,$71,$73,$20,$75,$77 ; Footer Line 2
	db $00
MenuPageOne::
	db $98,$21,"01 TAIWAN MAJOHN      ",$00
	db $98,$41,"02 JAPAN MAJOHN       ",$00
	db $98,$61,"03 HONG KONG MAJOHN   ",$00
	db $98,$81,"04 STOTRIS            ",$00
	db $98,$A1,"05 BOMB DISPOSER      ",$00
	db $98,$C1,"06 EXPLOSIVEW BRICK   ",$00
	db $98,$E1,"07 ARCTIC ZONE        ",$00
	db $99,$01,"08 ZOO BLOCK          ",$00
	db $99,$21,"09 PILE WONDER        ",$00
	db $99,$41,"10 TRAP & TURN        ",$00 ; Wrong '&' char
	db $99,$61,"11 RAILWAY            ",$00
	db $99,$81,"12 SNAKE ROY          ",$00
	db $99,$A1,"13 VIRUS ATTACK       ",$00
	db $99,$C1,"14 ELECTRONIC WORLD   ",$00
	db $99,$E1,"15 TRUBLE ZONE        ",$24

MenuPageTwo::
	db $98,$21,"16 DICE SQUARE        ",$00
	db $98,$41,"                      ",$00
	db $98,$61,"                      ",$00
	db $98,$81,"                      ",$00
	db $98,$A1,"                      ",$00
	db $98,$C1,"                      ",$00
	db $98,$E1,"                      ",$00
	db $99,$01,"                      ",$00
	db $99,$21,"                      ",$00
	db $99,$41,"                      ",$00
	db $99,$61,"                      ",$00
	db $99,$81,"                      ",$00
	db $99,$A1,"                      ",$00
	db $99,$C1,"                      ",$00
	db $99,$E1,"                      ",$24

Chinese1bpp::
INCBIN "gfx/chinese.1bpp"
Chinese1bpp_End:

GameName00::
	db $0C,$37,$01,$03,$21,$23,$20,$FF
GameName01::
	db $0A,$0E,$01,$02,$21,$23,$20,$FF
GameName02::
	db $1A,$26,$01,$02,$21,$23,$20,$FF
GameName03::
	db $10,$31,$09,$29,$FF
GameName04::
	db $14,$2D,$1F,$1B,$FF
GameName05::
	db $0D,$30,$29,$FF
GameName06::
	db $1C,$32,$09,$29,$FF
GameName07::
	db $1E,$16,$09,$29,$FF
GameName08::
	db $18,$2F,$24,$05,$FF
GameName09::
	db $28,$0F,$25,$FF
GameName10::
	db $35,$2A,$0B,$FF
GameName11::
	db $22,$04,$00,$33,$11,$FF
GameName12::
	db $1D,$19,$06,$13,$2F,$FF
GameName13::
	db $2B,$07,$15,$36,$2F,$FF
GameName14::
	db $2E,$12,$34,$10,$FF
GameName15::
	db $2C,$08,$17,$27,$FF
GameNameEOL::
	db $FF

LOC114C::
GameNamePtrTbl::
	dw GameName00
	dw GameName01
	dw GameName02
	dw GameName03
	dw GameName04
	dw GameName05
	dw GameName06
	dw GameName07
	dw GameName08
	dw GameName09
	dw GameName10
	dw GameName11
	dw GameName12
	dw GameName13
	dw GameName14
	dw GameName15
	dw GameNameEOL

LOC116E::
GameEntryPoints::
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0200
	dw $0000
	dw $0000
	dw $0000
	dw $0000

LOC118E::
GameEntryBases::
	db $14
	db $14
	db $14
	db $14
	db $02
	db $08
	db $12
	db $18
	db $10
	db $0A
	db $0C
	db $04
	db $1C
	db $1C
	db $1C
	db $1C

LOC119E::
GameEntryMasks::
	db $14
	db $14
	db $14
	db $14
	db $02
	db $08
	db $12
	db $18
	db $10
	db $0A
	db $0C
	db $04
	db $1C
	db $1C
	db $1C
	db $1C

LOC11AE::
SwitchRTN::
	pop hl
	ld a, $FF
	ld [$2000], a
	nop
	ld a, [$FF00 + $83] ; Base RB
	ld [$0000], a
	nop
	ld a, [$FF00 + $84] ; Mask
	ld [$4000], a
	nop
	xor a, a
	ld [$2000], a
	ld sp, $FFFE
	ld a, [$FF00 + $80] ; GB ID
	jp [hl]
LOC11CB::
	ld a, $30
	ld [$FF00 + $00], a
	ld a, $01
	ld [$FF00 + $4D], a
	xor a, a
	ld [$FF00 + $FF], a
	stop
	ld a, [$FF00 + $80]
	jp $0200

LOC11DD::
GameEntrySpecVals::
	db $02
	db $03
	db $04
	db $01
	db $04
	db $20
	db $10
	db $08
	db $02
	db $38
	db $04
	db $20
	db $80
	db $81
	db $82
	db $83

LOC11ED::
GameEntryPalPtrTbl::
	dw ColorTable + 9 * $8
	dw ColorTable + 9 * $8
	dw ColorTable + 9 * $8
	dw ColorTable + 9 * $8
	dw $0000
	dw $0000
	dw $0000
	dw ColorTable + 9 * $8
	dw ColorTable + 5 * $8
	dw $0000
	dw $0000
	dw $0000
	dw $0000
	dw $0000
	dw $0000
	dw $0000

LOC120D::
CopyBG_OBJ_Colors::
	ld a, [$FF00 + $80]
	cp a, $11
	ret nz
.wait_lyc_vblank:
	ld a, [$FF00 + $41] ; Whoops, should probably be $FF44
	cp a, $90
	jr nz, .wait_lyc_vblank
	ld a, $80
	ld c, $68
	ld b, $08
	ld [$FF00 + c], a
	inc c
.copy_bg:
	ldi a, [hl]
	ld [$FF00 + c], a
	dec b
	jr nz, .copy_bg
	inc c
	ld a, $80
	ld b, $08
	ld [$FF00 + c], a
	inc c
.copy_obj:
	ldi a, [hl]
	ld [$FF00 + c], a
	dec b
	jr nz, .copy_obj
	ret

LOC1232::
ShowIntro::
	ld de, SachenScroll_Logo
	call CopyBGData
	ld a, $E4
	ld c, $47
	ld [$FF00 + c], a
	ld a, $24
	inc c
	ld [$FF00 + c], a
	inc c
	ld [$FF00 + c], a
	ld a, $40
	ld [$FF00 + $42], a
.intro_scroll_loop:
.intro_wait_vblank:
	halt
	ld a, [$C011]
	and a, $01
	jr z, .intro_wait_vblank
	xor a, a
	ld [$C011], a
	call ReadKeypad
	ld a, [$C00F]
	and a, a
	jr nz, .intro_done
	ld hl, $C00A
	dec [hl]
	ldi a, [hl]
	and a, a
	jr z, .intro_done
	ld hl, $FF42
	ldi a, [hl]
	cp a, $C8
	jr z, .intro_scroll_over
	dec hl
	dec [hl]
	jr .intro_scroll_loop
.intro_scroll_over:
	call $1330
	jr .intro_scroll_loop
.intro_done:
	ld a, $E4
	ld [$FF00 + $48], a
	xor a, a
	ld [$C00F], a
	ld [$FF00 + $85], a
	ld hl, $8B00
	ld bc, $0400
	call ClearVRAM
	ld hl, $C100
	ld bc, $0100
	call ClearVRAM
	ld de, MenuHeaderFooter_PageOne
	call CopyBGData
	xor a, a
	ld [$FF00 + $83], a
	ld [$FF00 + $42], a
	ld [$C103], a
	ld a, $86
	ld [$FF00 + $84], a
	call DisplayGameName
	xor a, a
	ld [$FF00 + $83], a
	ld a, $86
	ld [$FF00 + $84], a
	ld a, $01
	ld [$FF00 + $81], a
	ld a, $5D ; Cursor Tile
	ld [$C102], a
	ld a, $08 ; Cursor X
	ld [$C101], a
	ld a, $18 ; Initial Cursor Y
	ld [$C100], a
	ld a, [$FF00 + $80]
	cp a, $11
	jr nz, .intro_not_cgb
.wait_lyc_vblank:
	ld a, [$FF00 + $41] ; Whoops, should probably be $FF44
	cp a, $90
	jr nz, .wait_lyc_vblank
	;; change BG0 col3 to black
	;; menu item color
	ld c, $68
	ld a, $86
	ld [$FF00 + c], a
	inc c
	xor a, a
	ld [$FF00 + c], a
	ld [$FF00 + c], a
	;; change OBJ0 col3 to plum
	;; cursor color
	ld a, $86
	ld c, $6A
	ld [$FF00 + c], a
	inc c
	ld a, $30
	ld [$FF00 + c], a
	ld [$FF00 + c], a
	;; Revert Xin1 Scroll BG pal
	ld a, $01
	ld [$FF00 + $4F], a
	ld hl, $9806
	ld bc, $0070
	call ClearVRAM
	;; GameName region BG2 pal
	ld hl, $9A00
	ld bc, $0040
	ld a, $02
	ld [$FF00 + $86], a
	call FillVRAM
	xor a, a
	ld [$FF00 + $4F], a
.intro_not_cgb:
	call PlayMusic
	ld a, $05
	ld [$FF00 + $FF], a
	ld [$FF00 + $0F], a
	ret

LOC1306::
ClearBG_OBJ_Colors::
	ld a, [$FF00 + $80]
	cp a, $11
	ret nz
	ld a, $80
	ld c, $68
	ld b, $40
	ld [$FF00 + c], a
	inc c
	xor a, a
	cpl
	call ClearColor
	ld a, $80
	ld b, $40
	inc c
	ld [$FF00 + c], a
	inc c
	ld a, $FF
	call ClearColor
	ret
ClearColor::
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	ld [$FF00 + c], a
	dec b
	jr nz, ClearColor
	ret

LOC1330::
ExecFSMFunc::
	ld a, [$C005]
	rla
	ld e, a
	xor a, a
	adc a, a
	ld d, a
	ld hl, FSMFuncOffTbl
	add hl, de
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld hl, FSMFuncOffTbl
	add hl, de
	jp [hl]
FSMFuncOffTbl::
	dw FSM0 - FSMFuncOffTbl
	dw FSM1 - FSMFuncOffTbl
	dw FSM2 - FSMFuncOffTbl
	
LOC134A::
FSM0::
	ld de, SachenScroll_Super
	call CopyBGData
	ld de, SachenScroll_16in1
	ld hl, $C006
	ld a, [de]
	ldi [hl], a
	inc de
	ld a, [de]
	ldi [hl], a
	inc de
	ld [hl], d
	inc hl
	ld [hl], e
	ld hl, $C100
	ld de, Xin1ScrollSprites
	ld bc, Xin1ScrollSprites_End - Xin1ScrollSprites + $12 ; Whoops 9 * 4 = 36 = 0x24, not 0x36
	call Copy
	xor a, a
	ld [$C000], a
	ld a, [$C005]
	inc a
	ld [$C005], a ; FSM0 --> FSM1
	ld a, $07
	ld [$FF00 + $85], a
	;; tiles that hide Xin1 Scroll
	ld hl, $8FC0
	ld bc, $0020
	call SetVRAM
	ld a, [$FF00 + $80]
	cp a, $11
	ret nz
.wait_lyc_vblank:
	ld a, [$FF00 + $44]
	cp a, $90
	jr nz, .wait_lyc_vblank
	;; Xin1 scroll region BG1 pal
	ld a, $01
	ld [$FF00 + $4F], a
	ld hl, $9806
	ld bc, $0070
	ld a, $01
	ld [$FF00 + $86], a
	call FillVRAM
	xor a, a
	ld [$FF00 + $4F], a
	ret

LOC13A3::
MoveSpritesDown::
	ld hl, $C0FC
	ld de, $0004
	ld bc, $0009
.mov_oam_down_loop:
	add hl, de
	ld a, [hl]
	cp a, $60
	ret z
	inc [hl]
	dec bc
	ld a, c
	or a, b
	jr nz, .mov_oam_down_loop
	ret

FSM1::
	ld a, [$FF00 + $85]
	inc a
	ld [$FF00 + $85], a
	ld hl, $C00A
	inc [hl]
	cp a, $08
	jr nz, MoveSpritesDown
	xor a, a
	ld [$FF00 + $85], a
	ld hl, $C009
	ldd a, [hl]
	ld e, a
	ldd a, [hl]
	ld d, a
	ldd a, [hl]
	ld c, a
	ldd a, [hl]
	ld b, a
	ld hl, $0000
	add hl, bc
.print_to_vram:
.wait_mode0:
	ld a, [$FF00 + $41]
	and a, $03
	jr nz, .wait_mode0
	ld a, [de]
	inc de
	and a, a
	jr z, .proc_item
	cp a, $24
	jr z, .print_fin
	ldi [hl], a
	jr .print_to_vram
.proc_item:
	ld hl, $C006
	ld a, [de]
	ldi [hl], a
	inc de
	ld a, [de]
	ldi [hl], a
	inc de
	ld [hl], d
	inc hl
	ld [hl], e
	ret
.print_fin:
	ld a, [$C005]
	inc a
	ld [$C005], a ; FSM1 --> FSM2
	ret
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

FSM2::
	ld a, [$C005]
	dec a
	ld [$C005], a ; FSM2 --> FSM1
	ret


LOC1416::
Xin1ScrollSprites::
	db $43,$38,$FD,$00
	db $43,$40,$FD,$00
	db $43,$48,$FD,$00
	db $43,$50,$FD,$00
	db $43,$58,$FD,$00
	db $43,$60,$FD,$00
	db $43,$68,$FD,$00
	db $43,$70,$FD,$00
	db $43,$78,$FD,$00
Xin1ScrollSprites_End:

LOC143A::
ColorTable::
	dw $7FFF,$7FFF,$7C00,$001F ; Menu BG0
	dw $7FFF,$7FFF,$03F0,$001F ; Menu BG1
	dw $7C00,$7FFF,$7FFF,$03FF ; Menu BG2
	dw $7FFF,$03E0,$03FF,$0000
	dw $0000,$3DEC,$62D3,$0000
	dw $7FFF,$7FD4,$FD08,$0000 ; Game BG0
	dw $0FFF,$1CE7,$F09F,$7C10 ; Game OBJ0
	dw $7FFF,$03E0,$03FF,$0000
	dw $7FFF,$62D3,$4D5A,$0000
	dw $7FFF,$03E0,$03FF,$0000 ; Game BG0
	dw $7FFF,$62D3,$4D5A,$0000 ; Game OBJ0

LOC1492::
Timer::
	push bc
	push de
	push hl
	push af
	call ProcessMusic
	pop af
	pop hl
	pop de
	pop bc
	ret

LOC149E::
SachenScrollGfx::
INCBIN "gfx/sachen_scroll.2bpp"
SachenScrollGfx_End:

LOC1B3E::
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	db $00

LOC1B5F::
PlayMusic::
	di
	ld hl, $C710
	ld c, $40
.clear_channel_data:
	xor a, a
	ldi [hl], a
	dec c
	jr nz, .clear_channel_data
	ld bc, MusicPtrTbl
	ld a, [$C706]
	sla a
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld l, a
	inc bc
	ld a, [bc]
	ld h, a
	ld bc, $0101
	ld de, $C700
.proc_channels:
	push hl
	ld a, [hl]
	and a, b
	jr nz, .copy_channel_data
	ld a, $FF
	ld [de], a
	ld a, b
	cp a, $02
	jr c, .mute_sound1
	jr z, .mute_sound2
	cp a, $04
	jr z, .mute_sound3
	;; Mute Sound Mode 4
	xor a, a
	ld [$FF00 + $21], a
	jr .proc_next_channel
.mute_sound1:
	xor a, a
	ld [$FF00 + $12], a
	jr .proc_next_channel
.mute_sound2:
	xor a, a
	ld [$FF00 + $17], a
	jr .proc_next_channel
.mute_sound3:
	xor a, a
	ld [$FF00 + $1A], a
	jr .proc_next_channel
.copy_channel_data:
	xor a, a
	ld [de], a
	push bc
	ld b, $00
	add hl, bc
	pop bc
	ld a, b
	cp a, $02
	jr c, .copy_sound1
	jr z, .copy_sound2
	cp a, $08
	jr c, .copy_sound3
	;; copy sound4
	ldi a, [hl]
	ld [$C740], a
	ldi a, [hl]
	ld [$C741], a
	jr .next_channel
.copy_sound1:
	ldi a, [hl]
	ld [$C710], a
	ldi a, [hl]
	ld [$C711], a
	jr .next_channel
.copy_sound2:
	ldi a, [hl]
	ld [$C720], a
	ldi a, [hl]
	ld [$C721], a
	jr .next_channel
.copy_sound3:
	ldi a, [hl]
	ld [$C730], a
	ldi a, [hl]
	ld [$C731], a
.next_channel:
	inc c
	inc c
.proc_next_channel:
	inc de
	sla b
	pop hl
	ld a, b
	cp a, $10
	jr c, .proc_channels
	ld a, $04
	ld [$FF00 + $07], a
	ld a, $C1
	ld [$FF00 + $06], a
	ld a, $FF
	ld [$C709], a
	ld [$C70A], a
	ld [$C70B], a
	ld [$C70C], a
	ld a, $80
	ld [$FF00 + $26], a
	ei
	ret

LOC1C09::
ProcessMusic::
	ld hl, $C707
	inc [hl]
	ld a, [hl]
	and a, $03
	cp a, $01
	jr c, .check_channel1
	jr z, .check_channel2
	cp a, $02
	jr z, .check_channel3
	ld a, [$C703]
	cp a, $FF
	ret z
	call processChannel4
	ret
.check_channel1:
	ld a, [$C700]
	cp a, $FF
	ret z
	call processChannel1
	ret
.check_channel2:
	ld a, [$C701]
	cp a, $FF
	ret z
	call processChannel2
	ret
.check_channel3:
	ld a, [$C702]
	cp a, $FF
	ret z
	call processChannel3
	ret
processChannel1::
	ld a, [$C713]
	and a, a
	jr z, .chDlyFin
	dec a
	ld [$C713], a
	ret nz
.chDlyFin:
	ld a, [$C717]
	and a, a
	jp nz, .procWaveData
.procCh:
	ld a, [$C710]
	ld l, a
	ld a, [$C711]
	ld h, a
	ld b, $00
	ld a, [$C712]
	ld c, a
	add hl, bc
.procChData:
	ld a, [hl]
	cp a, $FE
	jr z, .codeFE_TIMA
	jr nc, .codeFF_ChSel_Volume
	cp a, $FC
	jr c, .regular_wave_ix
	jr z, .codeFC_stop_loop
	; code 0xFD ???
	call IncCntCh1
	ld a, [hl]
	and a, $0F
	ld [$C719], a
	call IncCntCh1
	jr .procChData
.codeFF_ChSel_Volume:
	call IncCntCh1
	ld a, [hl]
	ld [$C71A], a
	call IncCntCh1
	ld a, [hl]
	ld [$C705], a
	call IncCntCh1
	ld a, [hl]
	ld [$C704], a
	call IncCntCh1
	jr .procChData
.codeFE_TIMA:
	call IncCntCh1
	ld a, [hl]
	ld [$C708], a
	add a, $49
	ld [$FF00 + $06], a
	call IncCntCh1
	jr .procChData
.codeFC_stop_loop:
	call IncCntCh1
	ld a, [hl]
	and a, a
	jr z, .rstCntCh
	; disable channel 1
	ld a, $FF
	ld [$C700], a
	xor a, a
	ld [$FF00 + $12], a
	ret
.rstCntCh:
	xor a, a
	ld [$C712], a
	jr .procCh
.regular_wave_ix:
	ld [$C715], a
	call IncCntCh1
	; process wave data
	xor a, a
	ld [$C716], a
	inc a
	ld [$C717], a
.procWaveData:
	call GetWavePtr
	ld a, [$C715]
	bit 7, a
	jr z, .add_no_overflow
	inc b
.add_no_overflow:
	sla a
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld l, a
	inc bc
	ld a, [bc]
	ld h, a
	ld a, [$C716]
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld [$C713], a
	ld a, [hl]
	ld [$C71C], a
	ld a, [$C71B]
	and a, a
	jr nz, .skip_update
	call UpdateSettingsCh1
.skip_update:
	ld a, [$C716]
	add a, $02
	ld [$C716], a
	; peek at next byte and exit if 0x00
	inc hl
	ld a, [hl]
	and a, a
	ret nz
	xor a, a
	ld [$C717], a
	ret
processChannel2::
	ld a, [$C723]
	and a, a
	jr z, .chDlyFin
	dec a
	ld [$C723], a
	ret nz
.chDlyFin:
	ld a, [$C727]
	and a, a
	jp nz, .procWaveData
.procCh:
	ld a, [$C720]
	ld l, a
	ld a, [$C721]
	ld h, a
	ld b, $00
	ld a, [$C722]
	ld c, a
	add hl, bc
.procChData:
	ld a, [hl]
	cp a, $FE
	jr z, .codeFE_TIMA
	jr nc, .codeFF_ChSel_Volume
	cp a, $FC
	jr c, .regular_wave_ix
	jr z, .codeFC_stop_loop
	; code 0xFD ???
	call IncCntCh2
	ld a, [hl]
	and a, $0F
	ld [$C729], a
	call IncCntCh2
	jr .procChData
.codeFF_ChSel_Volume:
	call IncCntCh2
	ld a, [hl]
	ld [$C72A], a
	call IncCntCh2
	ld a, [hl]
	ld [$C705], a
	call IncCntCh2
	ld a, [hl]
	ld [$C704], a
	call IncCntCh2
	jr .procChData
.codeFE_TIMA:
	call IncCntCh2
	ld a, [hl]
	ld [$C708], a
	add a, $49
	ld [$FF00 + $06], a
	call IncCntCh2
	jr .procChData
.codeFC_stop_loop:
	call IncCntCh2
	ld a, [hl]
	and a, a
	jr z, .rstCntCh
	; disable channel 2
	ld a, $FF
	ld [$C701], a
	xor a, a
	ld [$FF00 + $17], a
	ret
.rstCntCh:
	xor a, a
	ld [$C722], a
	jr .procCh
.regular_wave_ix:
	ld [$C725], a
	call IncCntCh2
	; process wave data
	xor a, a
	ld [$C726], a
	inc a
	ld [$C727], a
.procWaveData:
	call GetWavePtr
	ld a, [$C725]
	bit 7, a
	jr z, .add_no_overflow
	inc b
.add_no_overflow:
	sla a
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld l, a
	inc bc
	ld a, [bc]
	ld h, a
	ld a, [$C726]
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld [$C723], a
	ld a, [hl]
	ld [$C72C], a
	ld a, [$C72B]
	and a, a
	jr nz, .skip_update
	call UpdateSettingsCh2
.skip_update:
	ld a, [$C726]
	add a, $02
	ld [$C726], a
	; peek at next byte and exit if 0x00
	inc hl
	ld a, [hl]
	and a, a
	ret nz
	xor a, a
	ld [$C727], a
	ret
processChannel3::
	ld a, [$C733]
	and a, a
	jr z, .chDlyFin
	dec a
	ld [$C733], a
	ret nz
.chDlyFin:
	ld a, [$C737]
	and a, a
	jp nz, .procWaveData
.procCh:
	ld a, [$C730]
	ld l, a
	ld a, [$C731]
	ld h, a
	ld b, $00
	ld a, [$C732]
	ld c, a
	add hl, bc
.procChData:
	ld a, [hl]
	cp a, $FE
	jr z, .codeFE_TIMA
	jr nc, .codeFF_ChSel_Volume
	cp a, $FC
	jr c, .regular_wave_ix
	jr z, .codeFC_stop_loop
	; code 0xFD ???
	call IncCntCh3
	ld a, [hl]
	and a, $0F
	ld [$C739], a
	call IncCntCh3
	jr .procChData
.codeFF_ChSel_Volume:
	call IncCntCh3
	ld a, [hl]
	ld [$C73A], a
	call IncCntCh3
	ld a, [hl]
	ld [$C705], a
	call IncCntCh3
	ld a, [hl]
	ld [$C704], a
	call IncCntCh3
	jr .procChData
.codeFE_TIMA:
	call IncCntCh3
	ld a, [hl]
	ld [$C708], a
	add a, $49
	ld [$FF00 + $06], a
	call IncCntCh3
	jr .procChData
.codeFC_stop_loop:
	call IncCntCh3
	ld a, [hl]
	and a, a
	jr z, .rstCntCh
	; disable channel 3
	ld a, $FF
	ld [$C702], a
	xor a, a
	ld [$FF00 + $1A], a
	ret
.rstCntCh:
	xor a, a
	ld [$C732], a
	jr .procCh
.regular_wave_ix:
	ld [$C735], a
	call IncCntCh3
	; process wave data
	xor a, a
	ld [$C736], a
	inc a
	ld [$C737], a
.procWaveData:
	call GetWavePtr
	ld a, [$C735]
	bit 7, a
	jr z, .add_no_overflow
	inc b
.add_no_overflow:
	sla a
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld l, a
	inc bc
	ld a, [bc]
	ld h, a
	ld a, [$C736]
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld [$C733], a
	ld a, [hl]
	ld [$C73C], a
	ld a, [$C73B]
	and a, a
	jr nz, .skip_update
	call UpdateSettingsCh3
.skip_update:
	ld a, [$C736]
	add a, $02
	ld [$C736], a
	; peek at next byte and exit if 0x00
	inc hl
	ld a, [hl]
	and a, a
	ret nz
	xor a, a
	ld [$C737], a
	ret
processChannel4::
	ld a, [$C743]
	and a, a
	jr z, .chDlyFin
	dec a
	ld [$C743], a
	ret nz
.chDlyFin:
	ld a, [$C747]
	and a, a
	jp nz, .procWaveData
.procCh:
	ld a, [$C740]
	ld l, a
	ld a, [$C741]
	ld h, a
	ld b, $00
	ld a, [$C742]
	ld c, a
	add hl, bc
.procChData:
	ld a, [hl]
	cp a, $FE
	jr z, .codeFE_TIMA
	jr nc, .codeFF_ChSel_Volume
	cp a, $FC
	jr c, .regular_wave_ix
	jr z, .codeFC_stop_loop
	call IncCntCh4
	ld a, [hl]
	and a, $0F
	ld [$C749], a
	call IncCntCh4
	jr .procChData
.codeFF_ChSel_Volume:
	call IncCntCh4
	ld a, [hl]
	ld [$C74A], a
	call IncCntCh4
	ld a, [hl]
	ld [$C705], a
	call IncCntCh4
	ld a, [hl]
	ld [$C704], a
	call IncCntCh4
	jr .procChData
.codeFE_TIMA:
	call IncCntCh4
	ld a, [hl]
	ld [$C708], a
	add a, $49
	ld [$FF00 + $06], a
	call IncCntCh4
	jr .procChData
.codeFC_stop_loop:
	call IncCntCh4
	ld a, [hl]
	and a, a
	jr z, .rstCntCh
	; disable channel 4
	ld a, $FF
	ld [$C703], a
	xor a, a
	ld [$FF00 + $21], a
	ret
.rstCntCh:
	xor a, a
	ld [$C742], a
	jr .procCh
.regular_wave_ix:
	ld [$C745], a
	call IncCntCh4
	; process wave data
	xor a, a
	ld [$C746], a
	inc a
	ld [$C747], a
.procWaveData:
	call GetWavePtr
	ld a, [$C745]
	bit 7, a
	jr z, .add_no_overflow
	inc b
.add_no_overflow:
	sla a
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld l, a
	inc bc
	ld a, [bc]
	ld h, a
	ld a, [$C746]
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld [$C743], a
	ld a, [hl]
	ld [$C74C], a
	ld a, [$C74B]
	and a, a
	jr nz, .skip_update
	call UpdateSettingsCh4
.skip_update:
	ld a, [$C746]
	add a, $02
	ld [$C746], a
	; peek at next byte and exit if 0x00
	inc hl
	ld a, [hl]
	and a, a
	ret nz
	xor a, a
	ld [$C747], a
	ret
GetWavePtr::
	ld hl, WavePtrTbl
	ld a, [$C706]
	sla a
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld c, a
	ldi a, [hl]
	ld b, a
	ret
IncCntCh1:
	inc hl
	ld a, [$C712]
	inc a
	ld [$C712], a
	ret
IncCntCh2:
	inc hl
	ld a, [$C722]
	inc a
	ld [$C722], a
	ret
IncCntCh3:
	inc hl
	ld a, [$C732]
	inc a
	ld [$C732], a
	ret
IncCntCh4:
	inc hl
	ld a, [$C742]
	inc a
	ld [$C742], a
	ret
UpdateSettingsCh1::
	ld a, [$C71C]
	and a, a
	jr nz, .isEnabled
	ld [$FF00 + $12], a
	ret
.isEnabled:
	xor a, a
	ld [$FF00 + $10], a
	ld a, [$C719]
	swap a
	ld [$FF00 + $12], a
	ld a, [$C71A]
	and a, $03
	rrca
	rrca
	ld [$FF00 + $11], a
	ld bc, FreqLoTbl
	ld a, [hl]
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld [$FF00 + $13], a
	ld bc, FreqHiTbl
	ld a, [hl]
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	or a, $80
	ld [$FF00 + $14], a
	ld a, [$C704]
	ld [$FF00 + $24], a
	ld a, [$C705]
	ld [$FF00 + $25], a
	ret
UpdateSettingsCh2::
	ld a, [$C72C]
	and a, a
	jr nz, .isEnabled
	ld [$FF00 + $17], a
	ret
.isEnabled:
	ld a, [$C72A]
	and a, $03
	rrca
	rrca
	ld [$FF00 + $16], a
	ld a, [$C729]
	swap a
	ld [$FF00 + $17], a
	ld bc, FreqLoTbl
	ld a, [hl]
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld [$FF00 + $18], a
	ld bc, FreqHiTbl
	ld a, [hl]
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	or a, $80
	ld [$FF00 + $19], a
	ld a, [$C704]
	ld [$FF00 + $24], a
	ld a, [$C705]
	ld [$FF00 + $25], a
	ret
UpdateSettingsCh3::
	ld a, [$C73C]
	and a, a
	jr nz, .isEnabled
	ld [$FF00 + $1A], a
	ret
.isEnabled:
	ld a, $80
	ld [$FF00 + $1A], a
	xor a, a
	ld [$FF00 + $1B], a
	ld a, [$C73A]
	and a, $03
	and a, a
	jr nz, .no_underflow
	inc a
.no_underflow:
	rrca
	rrca
	rrca
	ld c, a
	ld a, [$C739]
	or a, c
	bit 5, a
	jr z, .no_force_level
	xor a, $40
.no_force_level:
	ld [$FF00 + $1C], a
	ld bc, FreqHiTbl
	ld a, [hl]
	sub a, $18
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	or a, $80
	ld [$FF00 + $1E], a
	ld bc, FreqLoTbl
	ld a, [hl]
	sub a, $18
	add a, c
	ld c, a
	ld a, $00
	adc a, b
	ld b, a
	ld a, [bc]
	ld [$FF00 + $1D], a
	ld a, [$C704]
	ld [$FF00 + $24], a
	ld a, [$C705]
	ld [$FF00 + $25], a
	ret
UpdateSettingsCh4::
	ld a, [$C74C]
	and a, a
	jr nz, .isEnabled
	ld [$FF00 + $21], a
	ret
.isEnabled:
	sub a, $25
	ld c, a
	sla a
	add a, c
	ld c, a
	ld b, $00
	push hl
	ld hl, SoundMode4Settings
	add hl, bc
	ldi a, [hl]
	ld [$FF00 + $20], a
	ldi a, [hl]
	swap a
	ld [$FF00 + $22], a
	ldi a, [hl]
	swap a
	ld [$FF00 + $21], a
	ld a, $C0
	ld [$FF00 + $23], a
	pop hl
	ld a, [$C704]
	ld [$FF00 + $24], a
	ld a, [$C705]
	ld [$FF00 + $25], a
	ret
LOC20AE::
	;; Unused Code
	ld a, [$C709]
	cp a, $FF
	jr z, .ch1_disabled
	ld c, a
	sla a
	sla a
	add a, c
	ld hl, GlobalSoundSettingTbl
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld [$FF00 + $10], a
	ldi a, [hl]
	swap a
	ld [$FF00 + $12], a
	ldi a, [hl]
	ld [$FF00 + $11], a
	and a, $3F
	ld [$C71B], a
	ldi a, [hl]
	ld [$FF00 + $13], a
	ldi a, [hl]
	ld [$FF00 + $14], a
	ld a, [$FF00 + $25]
	or a, $11
	ld [$FF00 + $25], a
	ld [$C705], a
	ld a, $FF
	ld [$C709], a
	ld [$FF00 + $24], a
	ld [$C704], a
	ld a, $80
	ld [$FF00 + $26], a
	jr .check_ch2
.ch1_disabled:
	ld a, [$C71B]
	cp a, $00
	jr z, .check_ch2
	dec a
	ld [$C71B], a
	jr nz, .check_ch2
	ld [$FF00 + $12], a
	ld a, [$C700]
	cp a, $FF
	jr z, .check_ch2
	ld a, [$C713]
	cp a, $02
	jr c, .check_ch2
	call UpdateSettingsCh1
.check_ch2:
	ld a, [$C70A]
	cp a, $FF
	jr z, .ch2_disabled
	sla a
	sla a
	ld hl, GlobalSoundSettingTbl
	ld c, a
	ld b, $00
	add hl, bc
	ldi a, [hl]
	ld [$FF00 + $16], a
	and a, $3F
	ld [$C72B], a
	ldi a, [hl]
	swap a
	ld [$FF00 + $17], a
	ldi a, [hl]
	ld [$FF00 + $18], a
	ldi a, [hl]
	ld [$FF00 + $19], a
	ld a, [$FF00 + $25]
	or a, $22
	ld [$FF00 + $25], a
	ld [$C705], a
	ld a, $FF
	ld [$C70A], a
	ld [$FF00 + $24], a
	ld [$C704], a
	ld a, $80
	ld [$FF00 + $26], a
	jr .check_ch3
.ch2_disabled:
	ld a, [$C72B]
	cp a, $00
	jr z, .check_ch3
	dec a
	ld [$C72B], a
	jr nz, .check_ch3
	ld [$FF00 + $17], a
	ld a, [$C701]
	cp a, $FF
	jr z, .check_ch3
	ld a, [$C723]
	cp a, $02
	jr c, .check_ch3
	call UpdateSettingsCh2
.check_ch3:
	ld a, [$C70B]
	cp a, $FF
	jr z, .ch3_disabled
	sla a
	sla a
	ld c, a
	ld b, $00
	ld hl, GlobalSoundSettingTbl
	add hl, bc
	ld a, $80
	ld [$FF00 + $1A], a
	ldi a, [hl]
	ld [$FF00 + $1B], a
	ld [$C73B], a
	ldi a, [hl]
	ld [$FF00 + $1E], a
	ldi a, [hl]
	ld [$FF00 + $1D], a
	ld a, [$FF00 + $25]
	or a, $44
	ld [$FF00 + $25], a
	ld a, $FF
	ld [$C70B], a
	ld [$FF00 + $24], a
	ld a, $80
	ld [$FF00 + $26], a
	jr .check_ch4
.ch3_disabled:
	ld a, [$C73B]
	cp a, $00
	jr z, .check_ch4
	dec a
	ld [$C73B], a
	jr nz, .check_ch4
	ld [$FF00 + $1A], a
	ld a, [$C702]
	cp a, $FF
	jr z, .check_ch4
	ld a, [$C733]
	cp a, $02
	jr c, .check_ch4
	call UpdateSettingsCh3
.check_ch4:
	ld a, [$C70C]
	cp a, $FF
	jr z, .ch4_disabled
	ld c, a
	sla a
	add a, c
	ld c, a
	ld b, $00
	ld hl, GlobalSoundSettingTbl
	add hl, bc
	ldi a, [hl]
	ld [$FF00 + $20], a
	ld [$C74B], a
	ldi a, [hl]
	swap a
	ld [$FF00 + $22], a
	ldi a, [hl]
	swap a
	ld [$FF00 + $21], a
	ld a, $80
	ld [$FF00 + $23], a
	ld a, [$FF00 + $25]
	or a, $88
	ld [$FF00 + $25], a
	ld a, $FF
	ld [$FF00 + $24], a
	ld [$C70C], a
	ld a, $80
	ld [$FF00 + $26], a
	ret
.ch4_disabled:
	ld a, [$C74B]
	cp a, $00
	ret z
	dec a
	ld [$C74B], a
	ret nz
	ld [$FF00 + $21], a
	ld a, [$C703]
	cp a, $FF
	ret z
	ld a, [$C743]
	cp a, $02
	ret c
	call UpdateSettingsCh4
	ret

LOC2219::
FreqLoTbl::
	db $00,$76,$E3,$49,$A9,$04,$5A,$AC
	db $F8,$40,$85,$C5,$02,$3B,$71,$A4
	db $D5,$02,$2D,$56,$7C,$A0,$C2,$E3
	db $01,$1E,$39,$52,$6A,$81,$97,$AB
	db $BE,$D0,$E1,$F1,$00,$0F,$1C,$29
	db $35,$41,$4B,$55,$5F,$68,$70,$79
	db $80,$87,$8E,$95,$9B,$A0,$A6,$AB
	db $B0,$B4,$B8,$BC,$C0,$C4,$C7,$CA
	db $CD,$D0,$D3,$D5,$D8,$DA,$DC,$DE
	db $E0

LOC2262::
FreqHiTbl::
	db $00,$00,$00,$01,$01,$02,$02,$02
	db $02,$03,$03,$03,$04,$04,$04,$04
	db $04,$05,$05,$05,$05,$05,$05,$05
	db $06,$06,$06,$06,$06,$06,$06,$06
	db $06,$06,$06,$06,$07,$07,$07,$07
	db $07,$07,$07,$07,$07,$07,$07,$07
	db $07,$07,$07,$07,$07,$07,$07,$07
	db $07,$07,$07,$07,$07,$07,$07,$07
	db $07,$07,$07,$07,$07,$07,$07,$07
	db $07

LOC22AB::
SoundMode4Settings::
	db $38,$22,$8F
	db $3B,$C0,$8F
	db $3B,$A1,$2F
	db $2D,$B1,$2F
	db $2B,$50,$8F
	db $3A,$D3,$2F
	db $37,$01,$2F
	db $37,$E5,$2F
	db $35,$81,$8F

LOC22C6::
MusicPtrTbl::
	dw Track0
	dw Track1
LOC22CA::
WavePtrTbl::
	dw WaveData0
	dw WaveData1

LOC22CE::
GlobalSoundSettingTbl::
	;; Table is gone
WaveData0::
	dw WaveData0_Wave00
	dw WaveData0_Wave01
	dw WaveData0_Wave02
	dw WaveData0_Wave03
	dw WaveData0_Wave04
	dw WaveData0_Wave05
	dw WaveData0_Wave06
	dw WaveData0_Wave07
	dw WaveData0_Wave08
	dw WaveData0_Wave09
	dw WaveData0_Wave10
	dw WaveData0_Wave11
	dw WaveData0_Wave12
	dw WaveData0_Wave13
	dw WaveData0_Wave14
	dw WaveData0_Wave15
	dw WaveData0_Wave16
	dw WaveData0_Wave17
	dw WaveData0_Wave18
	dw WaveData0_Wave19
	dw WaveData0_Wave20
	dw WaveData0_Wave21
	dw WaveData0_Wave22
	dw WaveData0_Wave23
	dw WaveData0_Wave24

WaveData0_Wave00::
	db $04,$1B
	db $04,$00
	db $04,$18
	db $04,$00
	db $04,$16
	db $04,$00
	db $04,$18
	db $04,$00
	db $00

WaveData0_Wave01::
	db $04,$14
	db $04,$00
	db $04,$11
	db $04,$00
	db $04,$0F
	db $04,$00
	db $04,$11
	db $04,$00
	db $00

WaveData0_Wave02::
	db $04,$1A
	db $04,$00
	db $04,$16
	db $04,$00
	db $04,$13
	db $04,$00
	db $04,$16
	db $04,$00
	db $00

WaveData0_Wave03::
	db $04,$1A
	db $04,$00
	db $04,$16
	db $0C,$00
	db $04,$11
	db $04,$00
	db $00

WaveData0_Wave04::
	db $04,$14
	db $04,$00
	db $04,$11
	db $04,$00
	db $04,$12
	db $04,$00
	db $04,$16
	db $04,$00
	db $00

WaveData0_Wave05::
	db $04,$18
	db $04,$00
	db $04,$1B
	db $14,$00
	db $00

WaveData0_Wave06::
	db $04,$2B
	db $02,$00
	db $02,$2E
	db $04,$2B
	db $02,$00
	db $02,$2E
	db $06,$00
	db $02,$2E
	db $04,$2B
	db $02,$00
	db $02,$2E
	db $00

WaveData0_Wave07::
	db $04,$2C
	db $04,$00
	db $03,$33
	db $01,$00
	db $04,$33
	db $10,$00
	db $00

WaveData0_Wave08::
	db $04,$2B
	db $02,$00
	db $02,$2E
	db $04,$2B
	db $02,$00
	db $02,$2E
	db $06,$00
	db $02,$2E
	db $04,$33
	db $02,$00
	db $02,$27
	db $00

WaveData0_Wave09::
	db $04,$29
	db $04,$00
	db $04,$2E
	db $14,$00
	db $00

WaveData0_Wave10::
	db $04,$2E
	db $04,$00
	db $04,$33
	db $02,$00
	db $02,$33
	db $04,$37
	db $04,$00
	db $04,$33
	db $04,$00
	db $00

WaveData0_Wave11::
	db $04,$2E
	db $02,$00
	db $01,$32
	db $01,$00
	db $04,$32
	db $02,$00
	db $02,$35
	db $08,$00
	db $04,$38
	db $02,$00
	db $02,$38
	db $00

WaveData0_Wave12::
	db $04,$37
	db $04,$00
	db $04,$37
	db $02,$00
	db $02,$33
	db $04,$36
	db $04,$00
	db $04,$35
	db $04,$00
	db $00

WaveData0_Wave13::
	db $04,$33
	db $1C,$00
	db $00

WaveData0_Wave14::
	db $04,$37
	db $04,$00
	db $04,$37
	db $02,$00
	db $02,$33
	db $04,$35
	db $04,$00
	db $04,$33
	db $04,$00
	db $00

WaveData0_Wave15::
	db $04,$38
	db $02,$00
	db $02,$38
	db $06,$37
	db $02,$38
	db $10,$00
	db $00

WaveData0_Wave16::
	db $04,$37
	db $04,$00
	db $06,$37
	db $02,$33
	db $05,$35
	db $01,$00
	db $02,$35
	db $06,$33
	db $02,$00
	db $00

WaveData0_Wave17::
	db $04,$32
	db $02,$00
	db $02,$32
	db $04,$33
	db $02,$00
	db $02,$2E
	db $10,$00
	db $00

WaveData0_Wave18::
	db $04,$37
	db $02,$00
	db $02,$35
	db $04,$33
	db $04,$00
	db $04,$37
	db $02,$00
	db $02,$35
	db $04,$33
	db $02,$00
	db $02,$37
	db $00

WaveData0_Wave19::
	db $04,$3A
	db $02,$00
	db $02,$3A
	db $04,$38
	db $02,$00
	db $02,$37
	db $04,$38
	db $04,$00
	db $04,$3C
	db $02,$00
	db $02,$38
	db $00

WaveData0_Wave20::
	db $04,$37
	db $02,$00
	db $02,$3A
	db $04,$3C
	db $02,$00
	db $02,$33
	db $04,$30
	db $02,$00
	db $02,$33
	db $04,$32
	db $02,$00
	db $02,$2E
	db $00

WaveData0_Wave21::
	db $04,$30
	db $04,$00
	db $04,$37
	db $14,$00
	db $00

WaveData0_Wave22::
	db $01,$2C
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$25
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$2C
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$25
	db $04,$00
	db $01,$2B
	db $02,$00
	db $00

WaveData0_Wave23::
	db $01,$2C
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$25
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$2C
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$2D
	db $07,$00
	db $00

WaveData0_Wave24::
	db $01,$2C
	db $05,$00
	db $01,$2B
	db $01,$00
	db $01,$2D
	db $0F,$00
	db $01,$2B
	db $07,$00
	db $00

LOC24C1::
Track0::
	db $0F             ; Ch1 + Ch2 + Ch3 + Ch4
	dw Track0_Channel1
	dw Track0_Channel2
	dw Track0_Channel3
	dw Track0_Channel4
Track0_Channel1::
	db $FD,$0F
	db $FE,$87
	db $FF,$03,$FF,$FF ; [75% duty cycle] [all channels on] [volume highest]
	db $00
	db $01
	db $00
	db $02
	db $00
	db $03
	db $04
	db $05
	db $FC,$00 ; loop
Track0_Channel2::
	db $FD,$0F
	db $FE,$87
	db $FF,$02,$FF,$FF ; [50% duty cycle] [all channels on] [volume highest]
	db $06
	db $07
	db $08
	db $09
	db $0A
	db $0B
	db $0C
	db $0D
	db $FC,$00 ; loop
Track0_Channel3::
	db $FD,$0F
	db $FE,$87
	db $FF,$03,$FF,$FF ; [75% duty cycle] [all channels on] [volume highest]
	db $0E
	db $0F
	db $10
	db $11
	db $12
	db $13
	db $14
	db $15
	db $FC,$00 ; loop
Track0_Channel4::
	db $FD,$0F
	db $FE,$87
	db $16
	db $16
	db $16
	db $17
	db $16
	db $16
	db $16
	db $18
	db $FC,$00 ; loop

LOC250E::
WaveData1::
	dw WaveData1_Wave00
	dw WaveData1_Wave01
WaveData1_Wave00::
	db $02,$25
	db $01,$00
	db $02,$28
	db $01,$00
	db $02,$25
	db $01,$00
	db $01,$29
	db $01,$2C
	db $02,$31
	db $00
WaveData1_Wave01::
	db $02,$29
	db $01,$00
	db $02,$2A
	db $01,$00
	db $02,$29
	db $01,$00
	db $04,$35
	db $00

LOC2534::
Track1::
	db $03             ; Ch1 + Ch2
	dw Track1_Channel1
	dw Track1_Channel2
Track1_Channel1::
	db $FD,$0F
	db $FE,$8F
	db $FF,$03,$FF,$FF ; [75% duty cycle] [all channels on] [volume highest]
	db $00
	db $FC,$01 ; stop
Track1_Channel2::
	db $FD,$0F
	db $FE,$8F
	db $FF,$03,$FF,$FF ; [75% duty cycle] [all channels on] [volume highest]
	db $01
	db $FC,$01 ; stop

SECTION "Bank1",ROMX[$7FFF],BANK[$1]
	db $00