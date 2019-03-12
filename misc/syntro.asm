// -- Syntro --
// 
// Small intro submission for the old skool 
// category at the Syntax Demo Party - Melbourne, 2014
//
// Platform: C64
// Released: Syntax#14
// Date: October/November, 2014
// Code: Jesder / 0xc64
// Site: http://www.0xc64.com
// Compiler: win2c64 (http://www.aartbik.com)
//
// Notes: Removed music and font data
//        Included details in the tables section towards
//        the end of the file
//

                        // zero page registers
.var REG_ZERO_02             = $02
.var REG_ZERO_03             = $03    // unused - floating point routine ptr low
.var REG_ZERO_04             = $04    // unused - floating point routine ptr high
.var REG_ZERO_2A             = $2a
.var REG_ZERO_52             = $52
.var REG_ZERO_FB             = $fb    // reserve for music player
.var REG_ZERO_FC             = $fc    // reserve for music player
.var REG_ZERO_FD             = $fd    // reserve for music player
.var REG_ZERO_FE             = $fe    // reserve for music player


                        // common register definitions
.var REG_INTSERVICE_LOW      = $0314    // interrupt service routine low byte
.var REG_INTSERVICE_HIGH     = $0315    // interrupt service routine high byte
.var REG_SCREENCTL_1         = $d011    // screen control register #1
.var REG_RASTERLINE          = $d012    // raster line position 
.var REG_SCREENCTL_2         = $d016    // screen control register #2
.var REG_MEMSETUP            = $d018    // memory setup register
.var REG_INTFLAG             = $d019    // interrupt flag register
.var REG_INTCONTROL          = $d01a    // interrupt control register
.var REG_BORCOLOUR           = $d020    // border colour register
.var REG_BGCOLOUR            = $d021    // background colour register
.var REG_INTSTATUS_1         = $dc0d    // interrupt control and status register #1
.var REG_INTSTATUS_2         = $dd0d    // interrupt control and status register #2

                        // standard memory locations
.var MEM_SCREEN              = $0400    // screen location
.var MEM_BITMAP              = $2000    // bitmap location
.var MEM_REG_CHARSET         = $d000    // character set rom
.var MEM_COLOURRAM           = $d800    // colour ram

                        // memory constants
.var C_LAST_SCREEN_LINE      = $077c    // last screen ram line
.var C_CHARSET               = $3000    // storage for character set
.var C_CHARSET_STARS         = $3200    // offset to star field characters
.var C_CHARSETCACHE          = $3800    // storage for modified regular character set
.var C_BITMAP_CACHE          = $4000    // bitmap unpack location
.var C_BITMAPSCREENPTR       = $4a00    // bitmap screen data pointer
.var C_BITMAPCOLOURPTR       = $4b40    // bitmap colour data pointer
.var C_STAR_COLOUR_CLONE_0   = $5000    // dark gray stars
.var C_STAR_COLOUR_CLONE_1   = $5190    // mid gray stars
.var C_STAR_COLOUR_CLONE_2   = $5320    // light gray stars
.var C_STAR_COLOUR_CLONE_3   = $54b0    // white stars
.var C_STAR_COLOUR_CLONE_4   = $5640    // white + light gray final
.var C_STAR_COLOUR_CLONE_5   = $57d0    // white + light gray + mid gray final
.var C_STAR_COLOUR_MAP_FINAL = $5960    // all final colours
.var C_STAR_CHARACTER_MAP    = $5af0    // starfield character map cache
.var C_MESSAGEPTR            = $5d00    // scroll text message pointer
.var C_TEXT_CYCLE_MSGS       = $6000    // text cycle messages pointer
.var C_COS_TABLE             = $6300    // cos table
.var C_SEQ_CELL_SCREEN_PTR   = $6400    // intro sequece screen cell pointers
.var C_MUSIC_LOCATION        = $c000    // music storage
.var C_MUSIC_INIT            = $c048    // music initialisation
.var C_MUSIC_PLAY            = $c021    // music update

                        // control constants (some...)
.var C_STAR_DELAY            = $02    // starfield animation smoothing
.var C_CYCLE_DELAY           = $02    // intro text colour smoothing
.var C_INTRO_SCROLL_DELAY    = $02    // intro text scroller smoothing
.var C_SEQUENCE_ADV_DELAY    = $07    // intro cell advance smoothing
.var C_INTRO_GLOW_DELAY      = $03    // intro cell glow smoothing

                        // program start
BasicUpstart2(start) // <- This creates a basic sys line that can start your program

                        *=$0810 "Begin"

                        // demo initialisation --------------------------------------------]
                        // ----------------------------------------------------------------]

                        // relocate music
start:                  ldx #00
music_relocate:         lda music_data, x
                        sta C_MUSIC_LOCATION, x
                        lda music_data + $100, x
                        sta C_MUSIC_LOCATION + $100, x
                        lda music_data + $200, x
                        sta C_MUSIC_LOCATION + $200, x
                        lda music_data + $300, x
                        sta C_MUSIC_LOCATION + $300, x
                        lda music_data + $400, x
                        sta C_MUSIC_LOCATION + $400, x
                        lda music_data + $500, x
                        sta C_MUSIC_LOCATION + $500, x
                        lda music_data + $600, x
                        sta C_MUSIC_LOCATION + $600, x
                        lda music_data + $700, x
                        sta C_MUSIC_LOCATION + $700, x
                        lda music_data + $800, x
                        sta C_MUSIC_LOCATION + $800, x
                        lda music_data + $900, x
                        sta C_MUSIC_LOCATION + $900, x
                        lda music_data + $a00, x
                        sta C_MUSIC_LOCATION + $a00, x
                        lda music_data + $b00, x
                        sta C_MUSIC_LOCATION + $b00, x
                        lda music_data + $b5d, x
                        sta C_MUSIC_LOCATION + $b5d, x
                        inx
                        bne music_relocate

                        // relocate intro cell screen pointers
                        ldx #00
init_cell_screen_ptrs:  lda seq_cell_screen_ptr, x
                        sta C_SEQ_CELL_SCREEN_PTR, x
                        inx
                        cpx #120
                        bne init_cell_screen_ptrs

                        // relocate cosine table
                        ldx #00
init_cos_table:         lda costable, x
                        sta C_COS_TABLE, x
                        inx
                        cpx #128
                        bne init_cos_table

                        // relocate text cycle messages
                        ldx #00
init_text_memory:       lda textcycle_messages_1, x
                        sta C_TEXT_CYCLE_MSGS, x
                        lda textcycle_messages_1 + $100, x
                        sta C_TEXT_CYCLE_MSGS + $100, x
                        lda textcycle_messages_1 + $11c, x
                        sta C_TEXT_CYCLE_MSGS + $11c, x
                        inx
                        bne init_text_memory

                        // relocate scroll message text
                        ldx #00
init_message_memory:    lda message, x
                        sta C_MESSAGEPTR, x
                        lda message + $100, x
                        sta C_MESSAGEPTR + $100, x
                        lda message + $1e1, x
                        sta C_MESSAGEPTR + $1e1, x
                        inx
                        bne init_message_memory

                        // initialise starfield colour map & character data
                        ldx #00
init_star_field_colmap: lda colour_patrn, x
                        sta C_STAR_COLOUR_MAP_FINAL, x
                        lda colour_patrn + $1e, x
                        sta C_STAR_COLOUR_MAP_FINAL + $28, x
                        lda colour_patrn + $23, x
                        sta C_STAR_COLOUR_MAP_FINAL + $50, x
                        lda colour_patrn + $19, x
                        sta C_STAR_COLOUR_MAP_FINAL + $78, x
                        lda colour_patrn + $32, x
                        sta C_STAR_COLOUR_MAP_FINAL + $a0, x
                        lda colour_patrn + $05, x
                        sta C_STAR_COLOUR_MAP_FINAL + $c8, x
                        lda colour_patrn + $0f, x
                        sta C_STAR_COLOUR_MAP_FINAL + $f0, x
                        lda colour_patrn + $28, x
                        sta C_STAR_COLOUR_MAP_FINAL + $118, x
                        lda colour_patrn + $14, x
                        sta C_STAR_COLOUR_MAP_FINAL + $140, x
                        lda colour_patrn + $2d, x
                        sta C_STAR_COLOUR_MAP_FINAL + $168, x

                        lda starmap_patrn1, x
                        sta C_STAR_CHARACTER_MAP, x
                        lda starmap_patrn2, x
                        sta C_STAR_CHARACTER_MAP + $28, x
                        lda starmap_patrn3, x
                        sta C_STAR_CHARACTER_MAP + $50, x
                        lda starmap_patrn1 + $03, x
                        sta C_STAR_CHARACTER_MAP + $78, x
                        lda starmap_patrn3 + $02, x
                        sta C_STAR_CHARACTER_MAP + $a0, x
                        lda starmap_patrn2 + $03, x
                        sta C_STAR_CHARACTER_MAP + $c8, x
                        lda starmap_patrn1 + $01, x
                        sta C_STAR_CHARACTER_MAP + $f0, x
                        lda starmap_patrn2 + $01, x
                        sta C_STAR_CHARACTER_MAP + $118, x
                        lda starmap_patrn3 + $01, x
                        sta C_STAR_CHARACTER_MAP + $140, x
                        lda starmap_patrn1 + $02, x
                        sta C_STAR_CHARACTER_MAP + $168, x
                        inx
                        cpx #40
                        bne init_star_field_colmap

                        // clone starfield colour map for fade in transition
                        lda #05
                        sta REG_ZERO_02
                        lda #00
                        sta REG_ZERO_2A

clone_star_field_map:   lda REG_ZERO_02
                        asl
                        tax
                        lda star_colour_ram_ptr, x
                        sta star_clone_map_1 + 1
                        lda star_colour_ram_ptr_2, x
                        sta star_clone_map_2 + 1
                        inx
                        lda star_colour_ram_ptr, x
                        sta star_clone_map_1 + 2
                        lda star_colour_ram_ptr_2, x
                        sta star_clone_map_2 + 2

                        ldy #00
clone_star_field_loop:  lda C_STAR_COLOUR_MAP_FINAL, y
                        lsr
                        clc
                        adc REG_ZERO_2A
                        tax
                        lda star_colour_ram_copy, x
star_clone_map_1:       sta C_STAR_COLOUR_CLONE_0, y
                        lda C_STAR_COLOUR_MAP_FINAL + $c8, y
                        lsr
                        clc
                        adc REG_ZERO_2A
                        tax
                        lda star_colour_ram_copy, x
star_clone_map_2:       sta C_STAR_COLOUR_CLONE_0, y
                        iny
                        cpy #200
                        bne clone_star_field_loop

                        clc
                        lda REG_ZERO_2A
                        adc #08
                        sta REG_ZERO_2A
                        dec REG_ZERO_02
                        bpl clone_star_field_map

                        // unpack bitmap into cache location
                        lda #$00 
                        sta unpack_literal + 1 
                        lda #>C_BITMAP_CACHE
                        sta unpack_literal + 2 
                        lda #<bitmap_data 
                        sta unpack_getbyte + 1
                        lda #>bitmap_data 
                        sta unpack_getbyte + 2 

unpack_next:            ldx #$01 
                        jsr unpack_getbyte 
                        cmp #$c2                // rle control code 
                        bne unpack_literal

                        jsr unpack_getbyte
                        cmp #$00                // rle control + 0 = end of data 
                        beq init_character_set

                        tax 
                        jsr unpack_getbyte

unpack_literal:         sta C_BITMAP_CACHE
                        inc unpack_literal + 1 
                        bne unpack_return2 
                        inc unpack_literal + 2 

unpack_return2:         dex
                        bne unpack_literal 
                        beq unpack_next

unpack_getbyte:         lda bitmap_data 
                        inc unpack_getbyte + 1 
                        bne unpack_return 
                        inc unpack_getbyte + 2 

unpack_return:          rts 

                        // initialise character set (64 chars / 512 bytes)
init_character_set:     ldx #00
                        lda char_set_data, x
                        sta C_CHARSET, x
                        sta REG_ZERO_02         // generate inverse byte
                        lda #$ff
                        sbc REG_ZERO_02
                        sta C_CHARSET + $400, x
                        lda char_set_data + $100, x
                        sta C_CHARSET + $100, x
                        sta REG_ZERO_02         // generate inverse byte
                        lda #$ff
                        sbc REG_ZERO_02
                        sta C_CHARSET + $500, x
                        inx
                        bne init_character_set + 2

                        // initialise starfield characters
                        ldx #00
init_star_chars:        lda char_set_data + $200, x
                        sta C_CHARSET + $200, x
                        lda char_set_data + $23c, x
                        sta C_CHARSET + $23c, x
                        inx
                        cpx #108
                        bne init_star_chars

                        // initialise bitmap memory
init_bitmap_memory:     ldx #00
                        lda #00
                        sta MEM_BITMAP, x
                        sta MEM_BITMAP + $100, x
                        sta MEM_BITMAP + $200, x
                        sta MEM_BITMAP + $300, x
                        sta MEM_BITMAP + $400, x
                        sta MEM_BITMAP + $500, x
                        sta MEM_BITMAP + $600, x
                        sta MEM_BITMAP + $700, x
                        sta MEM_BITMAP + $800, x
                        sta MEM_BITMAP + $900, x
                        sta MEM_BITMAP + $a00, x
                        sta MEM_BITMAP + $b00, x
                        inx
                        bne init_bitmap_memory + 4

                        // cache regular character set for intro transition
                        clc
                        sei
                        lda #$33
                        sta $01

                        ldx #00
copy_charset:           lda MEM_REG_CHARSET, x
                        sta C_CHARSETCACHE, x
                        lda MEM_REG_CHARSET + $100, x
                        sta C_CHARSETCACHE + $100, x
                        lda MEM_REG_CHARSET + $200, x
                        sta C_CHARSETCACHE + $200, x
                        lda MEM_REG_CHARSET + $300, x
                        sta C_CHARSETCACHE + $300, x
                        lda MEM_REG_CHARSET + $400, x
                        sta C_CHARSETCACHE + $400, x
                        lda MEM_REG_CHARSET + $500, x
                        sta C_CHARSETCACHE + $500, x
                        lda MEM_REG_CHARSET + $600, x
                        sta C_CHARSETCACHE + $600, x
                        lda MEM_REG_CHARSET + $700, x
                        sta C_CHARSETCACHE + $700, x
                        inx
                        bne copy_charset
                        lda #$37
                        sta $01
                        cli

                        // add custom intro characters to cached set
                        ldx #00
copy_custom_intro_chars: lda custom_intro_chars, x
                        sta C_CHARSETCACHE + $600, x
                        inx
                        cpx #64
                        bcc copy_custom_intro_chars


                        // initialise intro glow delay
                        ldx #C_INTRO_GLOW_DELAY
                        stx REG_ZERO_2A

                        // cache screen & background colour for intro transition
                        lda REG_BORCOLOUR
                        sta intro_glow_colours

                        lda REG_BGCOLOUR
                        sta bgcolour_cache
                        sta intro_back_glow_colours

                        // clear last screen line
                        lda #32
                        ldx #39
intro_clear_line:       sta C_LAST_SCREEN_LINE, x
                        dex
                        bpl intro_clear_line

                        // switch to modified cached char set
                        lda #030
                        sta REG_MEMSETUP

                        // initialise music
                        ldx #00
                        ldy #00
                        jsr C_MUSIC_INIT

                        // register first interrupt
                        sei                     // set up interrupt

                        lda #$7f
                        sta REG_INTSTATUS_1     // turn off the CIA interrupts
                        sta REG_INTSTATUS_2
                        and REG_SCREENCTL_1     // clear high bit of raster line
                        sta REG_SCREENCTL_1

                        ldy #00                 // sync start of demo to first scan line
                        sty REG_RASTERLINE

                        lda #<intro_glow_top
                        ldx #>intro_glow_top
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        lda #$01                // enable raster interrupts
                        sta REG_INTCONTROL
                        cli

forever:                bne forever

                        // play music update --------------------------------------------------]
                        // --------------------------------------------------------------------]
update_music_player:    inc REG_INTFLAG         // acknowledge interrupt
                        
                        jsr C_MUSIC_PLAY        // update music

set_postmusic_line:     ldy #242
                        sty REG_RASTERLINE
set_postmusic_low:      lda #<intro_glow_bottom
set_postmusic_high:     ldx #>intro_glow_bottom
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


 // ###############################################################################################]
 // ####
 // #### Intro sec. routines - glow border, transition away from BASIC screen, glow screen to black
 // ####
 // ###############################################################################################]

                        // intro transition glow -------------------------------------------------]
                        // -----------------------------------------------------------------------]

                        // begin intro glow at top of screen 
intro_glow_top:         inc REG_INTFLAG         // acknowledge interrupt
                        
                        ldx intro_glow_index
                        lda intro_glow_colours, x
                        sta REG_BORCOLOUR

                        ldy #60
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // intro glow over last screen line
intro_glow_bottom:      inc REG_INTFLAG         // acknowledge interrupt

                        ldy #252
                        sty REG_RASTERLINE

                        ldx intro_glow_index    
set_glowbottom_colptr:  lda intro_glow_colours, x
                        sta REG_BGCOLOUR

set_postglowbottom_low:  lda #<intro_update_glow
set_postglowbottom_high: ldx #>intro_update_glow
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81                        

                        // intro glow update -----------------------------------------------------]
                        // -----------------------------------------------------------------------]

                        // update intro glow effect
intro_update_glow:      inc REG_INTFLAG         // acknowledge interrupt
                        
                        dec REG_ZERO_2A         // apply glow smoothing
                        bne intro_glow_update_done

                        ldx #C_INTRO_GLOW_DELAY // reset smoothing
                        stx REG_ZERO_2A

                        ldx intro_glow_index    // glow complete?
                        cpx #25
                        bne intro_glow_update_done - 3

                        lda #<intro_update_fill // set new update interrupt pointers
                        ldx #>intro_update_fill 
                        sta set_postglowbottom_low + 1
                        stx set_postglowbottom_high + 1
                        lda #<intro_update_adv_seq
                        ldx #>intro_update_adv_seq
                        sta set_postmusic_low + 1
                        stx set_postmusic_high + 1
                        lda #115
                        sta set_postmusic_line + 1
                        jmp intro_glow_update_done

                        inc intro_glow_index    // advance border glow

intro_glow_update_done: ldy #00
                        sty REG_RASTERLINE
                        lda #<intro_glow_top
                        ldx #>intro_glow_top
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH   

                        lda bgcolour_cache      // restore background colour
                        sta REG_BGCOLOUR

                        jmp $ea81

                        // intro glow advance sequence -----------------------------------------]
                        // ---------------------------------------------------------------------]
intro_update_adv_seq:   inc REG_INTFLAG                 // acknowledge interrupt

                        dec sequence_advance_delay      // apply sequence advance delay
                        bne update_adv_seq_done

                        lda #C_SEQUENCE_ADV_DELAY       // reset delay
                        sta sequence_advance_delay

                        lda next_sequence_index         // detect if all sequences activated
                        cmp #15
                        bne intro_update_advance

                        ldy #<intro_glow_bottom // cut this update out of the interrupt sequence
                        ldx #>intro_glow_bottom
                        sty set_postmusic_low + 1
                        stx set_postmusic_high + 1
                        ldy #242
                        sty set_postmusic_line + 1
                        jmp update_adv_seq_done

intro_update_advance:   inc active_sequence_count       // advance active sequence counter

                        ldx #05        // clear new active cell list from last advance
                        lda #00
clear_new_active_cells: sta newly_active_cell_list, x
                        dex
                        bpl clear_new_active_cells

                        ldx sequence_cell_list_ptr      // get number of cells in this sequence
                        lda sequence_cell_list, x
                        sta newly_active_cell_count
                        sta REG_ZERO_03                 // store as counter

                        ldy #00
read_sequence_list:     inx
                        lda sequence_cell_list, x       // get cell to activate
                        sta newly_active_cell_list, y   // add to active cell list
                        iny
                        dec REG_ZERO_03                 // advance counter
                        bne read_sequence_list

                        inx                             // update sequence list pointer
                        stx sequence_cell_list_ptr

                        inc next_sequence_index

update_adv_seq_done:    ldy #242
                        sty REG_RASTERLINE

                        lda #<intro_glow_bottom
                        ldx #>intro_glow_bottom
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // intro update - fill & glow transition -----------------------------]
                        // -------------------------------------------------------------------]
intro_update_fill:      inc REG_INTFLAG         // acknowledge interrupt

                        lda bgcolour_cache
                        sta REG_BGCOLOUR

                        ldx newly_active_cell_count
                        cpx #00
                        beq update_transition_glow

                        dex
                        stx REG_ZERO_03
process_new_cells:      ldx REG_ZERO_03
                        lda newly_active_cell_list, x
                        asl
                        tay
                        lda C_SEQ_CELL_SCREEN_PTR, y
                        sta write_cell_address + 1
                        iny
                        lda C_SEQ_CELL_SCREEN_PTR, y
                        sta write_cell_address + 2

                        lda #00
                        sta REG_ZERO_52
                        sta cell_char_index
                        lda #03
                        sta REG_ZERO_02

clear_cell_block_y:     lda #03
                        sta REG_ZERO_2A
                        lda REG_ZERO_52
                        tax
clear_cell_block_x:     ldy cell_char_index
                        lda intro_block_def, y
write_cell_address:     sta $0400, x
                        inc cell_char_index 
                        inx
                        dec REG_ZERO_2A
                        bpl clear_cell_block_x

                        clc
                        lda REG_ZERO_52
                        adc #40
                        sta REG_ZERO_52

                        dec REG_ZERO_02
                        bpl clear_cell_block_y

                        dec REG_ZERO_03
                        bpl process_new_cells

                        lda #00                         // reset new cell count
                        sta newly_active_cell_count


update_transition_glow: ldx active_sequence_count       // apply glow to transition effect
                        cpx #00
                        beq update_transition_done

                        dec sequence_glow_delay         // apply glow delay
                        bne update_transition_done
                        lda #C_INTRO_GLOW_DELAY
                        sta sequence_glow_delay

                        stx REG_ZERO_02                 // number of sequences to process

                        ldx first_active_sequence       // starting sequence
                        stx REG_ZERO_03
                        
process_sequence_glow:  lda REG_ZERO_03
                        asl
                        tay
                        lda sequence_update_ptr, y      // get the sequence update pointers
                        sta execute_sequence_glow + 1
                        iny 
                        lda sequence_update_ptr, y
                        sta execute_sequence_glow + 2

                        ldx REG_ZERO_03
                        lda sequence_colour_index, x    // get colour to use on the sequence
                        tax
                        lda cell_glow_colours, x

                        ldx #03
execute_sequence_glow:  jsr update_sequence_0

                        ldx REG_ZERO_03
                        lda sequence_colour_index, x
                        tay
                        iny
                        cpy #08
                        bne continue_sequence_glow      // sequence complete ?
                        inc first_active_sequence       // advance first active sequence 
                        dec active_sequence_count       // lower active count

continue_sequence_glow: tya
                        sta sequence_colour_index, x

                        inc REG_ZERO_03
                        dec REG_ZERO_02
                        bne process_sequence_glow


                        ldx first_active_sequence       // all sequences complete?
                        cpx #15
                        bne update_transition_done

                        lda #00
                        sta intro_glow_index
                        lda #<update_intro_glow_back    // set new update pointers
                        ldx #>update_intro_glow_back
                        sta set_postglowbottom_low + 1
                        stx set_postglowbottom_high + 1

                        jsr setup_intro_final_glow

update_transition_done: ldy #60
                        sty REG_RASTERLINE

                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // intro - setup final glow -------------------------------------------]
                        // --------------------------------------------------------------------]
setup_intro_final_glow: ldx #39
                        lda #160
                        sta MEM_SCREEN + $3c0, x
                        lda #00
                        sta MEM_COLOURRAM + $3c0, x
                        dex
                        bpl setup_intro_final_glow + 2
                        lda #<intro_back_glow_colours
                        ldx #>intro_back_glow_colours
                        sta set_glowbottom_colptr + 1
                        stx set_glowbottom_colptr + 2
                        ldx #38
                        stx intro_glow_delay
                        rts

                        // intro - glow screen to black -------------------------------------]
                        // ------------------------------------------------------------------]
intro_glow_screen:      inc REG_INTFLAG         // acknowledge interrupt
                        
                        ldx intro_glow_index
                        lda intro_back_glow_colours, x
                        sta REG_BGCOLOUR

                        ldy #60
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // intro update - glow screen to black --------------------------------]
                        // --------------------------------------------------------------------]
update_intro_glow_back: inc REG_INTFLAG         // acknowledge interrupt
                        
                        dec intro_glow_delay    // apply smoothing
                        bne intro_glow_back_done

                        ldx #C_INTRO_GLOW_DELAY // reset smoothing
                        stx intro_glow_delay

                        ldx intro_glow_index    // glow complete?
                        cpx #23
                        bne intro_glow_back_done - 3

                        lda #215                 // set delay before before switch to intro text
                        sta intro_glow_delay

                        lda #<update_intro_delay
                        ldx #>update_intro_delay
                        sta set_postmusic_low + 1
                        stx set_postmusic_high + 1

                        jmp intro_glow_back_done

                        inc intro_glow_index
intro_glow_back_done:   ldy #00
                        sty REG_RASTERLINE
                        lda #<intro_glow_screen
                        ldx #>intro_glow_screen
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


update_intro_delay:     inc REG_INTFLAG

                        dec intro_glow_delay    // apply delay (resuse delay variable)
                        bne update_delay_done

                        lda #<init_intro_text
                        ldx #>init_intro_text
                        sta set_postmusic_low + 1
                        stx set_postmusic_high + 1
                        lda #100
                        sta set_postmusic_line + 1

update_delay_done:      ldy #60
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // intro fill subroutines --------------------------------------------]
                        // -------------------------------------------------------------------]

                        // sequence zero update 
update_sequence_0:      sta $d800, x
                        sta $d800 + $28, x
                        sta $d800 + $50, x
                        sta $d800 + $78, x
                        dex
                        bpl update_sequence_0
                        rts

                        // sequence one update 
update_sequence_1:      sta $d804, x
                        sta $d804 + $28, x
                        sta $d804 + $50, x
                        sta $d804 + $78, x
                        sta $d8a0, x
                        sta $d8a0 + $28, x
                        sta $d8a0 + $50, x
                        sta $d8a0 + $78, x
                        dex
                        bpl update_sequence_1
                        rts

                        // sequence two update 
update_sequence_2:      sta $d808, x
                        sta $d808 + $28, x
                        sta $d808 + $50, x
                        sta $d808 + $78, x
                        sta $d8a4, x
                        sta $d8a4 + $28, x
                        sta $d8a4 + $50, x
                        sta $d8a4 + $78, x
                        sta $d940, x
                        sta $d940 + $28, x
                        sta $d940 + $50, x
                        sta $d940 + $78, x
                        dex
                        bpl update_sequence_2
                        rts

                        // sequence three update 
update_sequence_3:      sta $d80c, x
                        sta $d80c + $28, x
                        sta $d80c + $50, x
                        sta $d80c + $78, x
                        sta $d8a8, x
                        sta $d8a8 + $28, x
                        sta $d8a8 + $50, x
                        sta $d8a8 + $78, x
                        sta $d944, x
                        sta $d944 + $28, x
                        sta $d944 + $50, x
                        sta $d944 + $78, x
                        sta $d9e0, x
                        sta $d9e0 + $28, x
                        sta $d9e0 + $50, x
                        sta $d9e0 + $78, x
                        dex
                        bpl update_sequence_3
                        rts

                        // sequence four update 
update_sequence_4:      sta $d810, x
                        sta $d810 + $28, x
                        sta $d810 + $50, x
                        sta $d810 + $78, x
                        sta $d8ac, x
                        sta $d8ac + $28, x
                        sta $d8ac + $50, x
                        sta $d8ac + $78, x
                        sta $d948, x
                        sta $d948 + $28, x
                        sta $d948 + $50, x
                        sta $d948 + $78, x
                        sta $d9e4, x
                        sta $d9e4 + $28, x
                        sta $d9e4 + $50, x
                        sta $d9e4 + $78, x
                        sta $da80, x
                        sta $da80 + $28, x
                        sta $da80 + $50, x
                        sta $da80 + $78, x
                        dex
                        bpl update_sequence_4
                        rts

                        // sequence five update 
update_sequence_5:      sta $d814, x
                        sta $d814 + $28, x
                        sta $d814 + $50, x
                        sta $d814 + $78, x
                        sta $d8b0, x
                        sta $d8b0 + $28, x
                        sta $d8b0 + $50, x
                        sta $d8b0 + $78, x
                        sta $d94c, x
                        sta $d94c + $28, x
                        sta $d94c + $50, x
                        sta $d94c + $78, x
                        sta $d9e8, x
                        sta $d9e8 + $28, x
                        sta $d9e8 + $50, x
                        sta $d9e8 + $78, x
                        sta $da84, x
                        sta $da84 + $28, x
                        sta $da84 + $50, x
                        sta $da84 + $78, x
                        sta $db20, x
                        sta $db20 + $28, x
                        sta $db20 + $50, x
                        sta $db20 + $78, x
                        dex
                        bpl update_sequence_5
                        rts

                        // sequence six update 
update_sequence_6:      sta $d818, x
                        sta $d818 + $28, x
                        sta $d818 + $50, x
                        sta $d818 + $78, x
                        sta $d8b4, x
                        sta $d8b4 + $28, x
                        sta $d8b4 + $50, x
                        sta $d8b4 + $78, x
                        sta $d950, x
                        sta $d950 + $28, x
                        sta $d950 + $50, x
                        sta $d950 + $78, x
                        sta $d9ec, x
                        sta $d9ec + $28, x
                        sta $d9ec + $50, x
                        sta $d9ec + $78, x
                        sta $da88, x
                        sta $da88 + $28, x
                        sta $da88 + $50, x
                        sta $da88 + $78, x
                        sta $db24, x
                        sta $db24 + $28, x
                        sta $db24 + $50, x
                        sta $db24 + $78, x
                        dex
                        bpl update_sequence_6
                        rts

                        // sequence seven update 
update_sequence_7:      sta $d81c, x
                        sta $d81c + $28, x
                        sta $d81c + $50, x
                        sta $d81c + $78, x
                        sta $d8b8, x
                        sta $d8b8 + $28, x
                        sta $d8b8 + $50, x
                        sta $d8b8 + $78, x
                        sta $d954, x
                        sta $d954 + $28, x
                        sta $d954 + $50, x
                        sta $d954 + $78, x
                        sta $d9f0, x
                        sta $d9f0 + $28, x
                        sta $d9f0 + $50, x
                        sta $d9f0 + $78, x
                        sta $da8c, x
                        sta $da8c + $28, x
                        sta $da8c + $50, x
                        sta $da8c + $78, x
                        sta $db28, x
                        sta $db28 + $28, x
                        sta $db28 + $50, x
                        sta $db28 + $78, x
                        dex
                        bpl update_sequence_7
                        rts

                        // sequence eight update 
update_sequence_8:      sta $d820, x
                        sta $d820 + $28, x
                        sta $d820 + $50, x
                        sta $d820 + $78, x
                        sta $d8bc, x
                        sta $d8bc + $28, x
                        sta $d8bc + $50, x
                        sta $d8bc + $78, x
                        sta $d958, x
                        sta $d958 + $28, x
                        sta $d958 + $50, x
                        sta $d958 + $78, x
                        sta $d9f4, x
                        sta $d9f4 + $28, x
                        sta $d9f4 + $50, x
                        sta $d9f4 + $78, x
                        sta $da90, x
                        sta $da90 + $28, x
                        sta $da90 + $50, x
                        sta $da90 + $78, x
                        sta $db2c, x
                        sta $db2c + $28, x
                        sta $db2c + $50, x
                        sta $db2c + $78, x
                        dex
                        bpl update_sequence_8
                        rts

                        // sequence nine update 
update_sequence_9:      sta $d824, x
                        sta $d824 + $28, x
                        sta $d824 + $50, x
                        sta $d824 + $78, x
                        sta $d8c0, x
                        sta $d8c0 + $28, x
                        sta $d8c0 + $50, x
                        sta $d8c0 + $78, x
                        sta $d95c, x
                        sta $d95c + $28, x
                        sta $d95c + $50, x
                        sta $d95c + $78, x
                        sta $d9f8, x
                        sta $d9f8 + $28, x
                        sta $d9f8 + $50, x
                        sta $d9f8 + $78, x
                        sta $da94, x
                        sta $da94 + $28, x
                        sta $da94 + $50, x
                        sta $da94 + $78, x
                        sta $db30, x
                        sta $db30 + $28, x
                        sta $db30 + $50, x
                        sta $db30 + $78, x
                        dex
                        bpl update_sequence_9
                        rts

                        // sequence ten update 
update_sequence_10:     sta $d8c4, x
                        sta $d8c4 + $28, x
                        sta $d8c4 + $50, x
                        sta $d8c4 + $78, x
                        sta $d960, x
                        sta $d960 + $28, x
                        sta $d960 + $50, x
                        sta $d960 + $78, x
                        sta $d9fc, x
                        sta $d9fc + $28, x
                        sta $d9fc + $50, x
                        sta $d9fc + $78, x
                        sta $da98, x
                        sta $da98 + $28, x
                        sta $da98 + $50, x
                        sta $da98 + $78, x
                        sta $db34, x
                        sta $db34 + $28, x
                        sta $db34 + $50, x
                        sta $db34 + $78, x
                        dex
                        bpl update_sequence_10
                        rts

                        // sequence eleven update 
update_sequence_11:     sta $d964, x
                        sta $d964 + $28, x
                        sta $d964 + $50, x
                        sta $d964 + $78, x
                        sta $da00, x
                        sta $da00 + $28, x
                        sta $da00 + $50, x
                        sta $da00 + $78, x
                        sta $da9c, x
                        sta $da9c + $28, x
                        sta $da9c + $50, x
                        sta $da9c + $78, x
                        sta $db38, x
                        sta $db38 + $28, x
                        sta $db38 + $50, x
                        sta $db38 + $78, x
                        dex
                        bpl update_sequence_11
                        rts

                        // sequence twelve update 
update_sequence_12:     sta $da04, x
                        sta $da04 + $28, x
                        sta $da04 + $50, x
                        sta $da04 + $78, x
                        sta $daa0, x
                        sta $daa0 + $28, x
                        sta $daa0 + $50, x
                        sta $daa0 + $78, x
                        sta $db3c, x
                        sta $db3c + $28, x
                        sta $db3c + $50, x
                        sta $db3c + $78, x
                        dex
                        bpl update_sequence_12
                        rts

                        // sequence thirteen update 
update_sequence_13:     sta $daa4, x
                        sta $daa4 + $28, x
                        sta $daa4 + $50, x
                        sta $daa4 + $78, x
                        sta $db40, x
                        sta $db40 + $28, x
                        sta $db40 + $50, x
                        sta $db40 + $78, x
                        dex
                        bpl update_sequence_13
                        rts

                        // sequence fourteen update 
update_sequence_14:     sta $db44, x
                        sta $db44 + $28, x
                        sta $db44 + $50, x
                        sta $db44 + $78, x
                        dex
                        bpl update_sequence_14
                        rts


 // ###################################################################################]
 // ####
 // #### Intro text routines - fade on text strings, scroll & fade off
 // ####
 // ###################################################################################]

                        // intro text effect setup --------------------------------------------]
                        // --------------------------------------------------------------------]
init_intro_text:        inc REG_INTFLAG

                        lda #028                        // switch to demo font
                        sta REG_MEMSETUP

                        lda #00
                        sta intro_text_col_index
                        sta intro_msg_timer

                        ldx #00     // clear screen / remove intro transition junk
                        lda #32
intro_text_init_clear:  sta $0400, x
                        sta $0500, x
                        sta $0600, x
                        sta $06e8, x
                        inx
                        bne intro_text_init_clear

                        ldx intro_message_index
                        jsr intro_copy_message          // render message
                        jsr intro_set_scroll_addr       

                        lda #<intro_rightscroll_start
                        ldy #>intro_rightscroll_start
                        sta set_postmusic_low + 1
                        sty set_postmusic_high + 1
                        
                        lda scroll_right_start, x
                        sta set_postmusic_line + 1

                        lda #30
                        sta REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // intro text scroll right ------------------------------------------------]
                        // ------------------------------------------------------------------------]
intro_rightscroll_start: inc REG_INTFLAG                 // acknowledge interrupt
                        clc
                        lda REG_SCREENCTL_2
                        and #248
                        adc right_scroll_magnitude      // apply scroll
                        sta REG_SCREENCTL_2

                        ldx intro_message_index         // determine right scroll ending raster
                        lda scroll_right_end, x
                        sta REG_RASTERLINE
                        lda #<intro_rightscroll_end
                        ldx #>intro_rightscroll_end
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


intro_rightscroll_end:  inc REG_INTFLAG                 // acknowledge interrupt
                        clc
                        lda REG_SCREENCTL_2
                        and #248                        // remove scroll
                        sta REG_SCREENCTL_2

                        ldx intro_message_index         // determine left scroll starting raster
                        lda scroll_left_start, x
                        sta REG_RASTERLINE

                        lda #<intro_leftscroll_start
                        ldx #>intro_leftscroll_start
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // intro text left scroll ----------------------------------------------]
                        // ---------------------------------------------------------------------]
intro_leftscroll_start: inc REG_INTFLAG                 // acknowledge interrupt
                        clc
                        lda REG_SCREENCTL_2
                        and #248
                        adc left_scroll_magnitude       // apply scroll
                        sta REG_SCREENCTL_2

                        ldx intro_message_index         // determine left scroll ending raster
                        lda scroll_left_end, x
                        sta REG_RASTERLINE
                        lda #<intro_leftscroll_end
                        ldx #>intro_leftscroll_end
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


intro_leftscroll_end:   inc REG_INTFLAG                 // acknowledge interrupt
                        clc
                        lda REG_SCREENCTL_2
                        and #248                        // remove scroll
                        sta REG_SCREENCTL_2

                        lda #252
                        sta REG_RASTERLINE
                        lda #<update_intro_text
                        ldx #>update_intro_text
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // intro text update ---------------------------------------------------]
                        // ---------------------------------------------------------------------]
update_intro_text:      inc REG_INTFLAG                 // acknowledge interrupt

                        // update scroller
update_scroll_delay:    dec scroll_delay
                        bne intro_update_colour_ram

                        lda #C_INTRO_SCROLL_DELAY       // reset scroll delay
                        sta scroll_delay

                        inc right_scroll_magnitude      // update scroll magnitudes
                        dec left_scroll_magnitude       
                        bpl intro_update_colour_ram

                        lda #07                         // reset scroll magnitudes at max
                        sta left_scroll_magnitude
                        lda #00
                        sta right_scroll_magnitude

                        ldx #00                         // shift characters for scroll
                        ldy #39
intro_shift_left_src:   lda $0401, x
intro_shift_left_dest:  sta $0400, x
intro_shift_right_src:  lda $0400, y
intro_shift_right_dest: sta $0401, y
                        inx
                        dey
                        bpl intro_shift_left_src

                        // update colour ram
intro_update_colour_ram: dec cycle_delay
                        bne update_timer
                        lda #C_CYCLE_DELAY
                        sta cycle_delay

                        inc intro_msg_timer

                        ldx #00                         // cycle colour ram
                        ldy #39
intro_left_col_src:     lda $d801, x
intro_left_col_dest:    sta $d800, x
intro_right_col_src:    lda $d800, y
intro_right_col_dest:   sta $d801, y
                        inx
                        dey
                        bpl intro_left_col_src

                        ldx intro_text_col_index
intro_text_getcol_ptr:  lda intro_text_colours_in, x
                        ldx #00
                        ldy #39
intro_new_left_col:     sta $07c0, y
intro_new_right_col:    sta $07c0, x

                        clc                             // update colour cycle
                        ldx intro_text_col_index
                        inx
                        cpx #16
                        bne update_timer - 3
                        ldx #15
                        stx intro_text_col_index

                        // detect fade off time
update_timer:           ldx intro_msg_timer
                        cpx #63
                        bne update_msg_detect_end
                        lda #00                         // reset colour table index
                        sta intro_text_col_index
                        lda #<intro_text_colours_out    // modify colour table pointer
                        sta intro_text_getcol_ptr + 1
                        lda #>intro_text_colours_out
                        sta intro_text_getcol_ptr + 2

update_msg_detect_end:  cpx #115
                        bne text_update_complete

                        ldx intro_message_index         // advance display message 
                        inx
                        cpx #06                         // detect all messages displayed
                        stx intro_message_index
                        bne intro_prep_message
                        
                        lda #<setup_main_demo_bitmap
                        sta set_postmusic_low + 1
                        lda #>setup_main_demo_bitmap
                        sta set_postmusic_high + 1
                        lda #120
                        sta set_postmusic_line + 1

                        jmp text_update_complete

intro_prep_message:     jsr intro_copy_message
                        jsr intro_set_scroll_addr

                        lda scroll_right_start, x // update post music jump for right scroll start
                        sta set_postmusic_line + 1

                        lda #C_CYCLE_DELAY              // reset cycle delay
                        sta cycle_delay

                        lda #00                         // reset colour table index
                        sta intro_text_col_index
                        sta intro_msg_timer

                        lda #<intro_text_colours_in     // reset colour table pointer
                        sta intro_text_getcol_ptr + 1
                        lda #>intro_text_colours_in
                        sta intro_text_getcol_ptr + 2


text_update_complete:   ldx intro_message_index
set_postupdate_line:    lda #30
                        sta REG_RASTERLINE
set_postupdate_low:     lda #<update_music_player
set_postupdate_high:    ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // intro text support routines ---------------------------------------]
                        // -------------------------------------------------------------------]
intro_set_scroll_addr:  txa
                        asl
                        tay
                        clc
                        lda left_message_scrram, y
                        sta intro_shift_left_dest + 1
                        adc #01
                        sta intro_shift_left_src + 1
                        lda left_message_scrram + 1, y
                        sta intro_shift_left_dest + 2
                        sta intro_shift_left_src + 2

                        lda right_message_scrram, y
                        sta intro_shift_right_src + 1
                        adc #01
                        sta intro_shift_right_dest + 1
                        lda right_message_scrram + 1, y
                        sta intro_shift_right_dest + 2
                        sta intro_shift_right_src + 2


                        lda left_message_colram, y
                        sta intro_left_col_dest + 1
                        sta intro_new_left_col + 1
                        adc #01
                        sta intro_left_col_src + 1
                        lda left_message_colram + 1, y
                        sta intro_left_col_dest + 2
                        sta intro_left_col_src + 2
                        sta intro_new_left_col + 2

                        lda right_message_colram, y
                        sta intro_right_col_src + 1
                        sta intro_new_right_col + 1
                        adc #01
                        sta intro_right_col_dest + 1
                        lda right_message_colram + 1, y
                        sta intro_right_col_dest + 2
                        sta intro_right_col_src + 2
                        sta intro_new_right_col + 2

                        rts

                        // copy intro message into screen ram
intro_copy_message:     txa
                        asl
                        tay

                        lda left_messages, y
                        sta readleftmessage + 1
                        lda left_messages + 1, y
                        sta readleftmessage + 2

                        lda right_messages, y
                        sta readrightmessage + 1
                        lda right_messages + 1, y
                        sta readrightmessage + 2

                        lda left_message_scrram, y
                        sta writeleftmessage + 1
                        lda left_message_scrram + 1, y
                        sta writeleftmessage + 2

                        lda right_message_scrram, y
                        sta writerightmessage + 1
                        lda right_message_scrram + 1, y
                        sta writerightmessage + 2

                        ldy #00
readleftmessage:        lda intro_messages, y
writeleftmessage:       sta MEM_SCREEN, y
readrightmessage:       lda intro_messages, y
writerightmessage:      sta MEM_SCREEN, y
                        iny
                        cpy #040
                        bne readleftmessage
                        rts


 // ###########################################################################################]
 // ####
 // #### Main demo routines - bitmap logo, rolling starfield, raster bars, text scroller
 // ####
 // ###########################################################################################]

                        // setup main demo section --------------------------------------------]
                        // --------------------------------------------------------------------]
setup_main_demo_bitmap: inc REG_INTFLAG         // acknowledge interrupt

                        ldx #00                 // initialise bitmap screen data & colour ram
                        ldy #00
init_bitmap:            lda C_BITMAPSCREENPTR, x         
                        sta MEM_SCREEN, x 
                        lda C_BITMAPSCREENPTR + $28, x   
                        sta MEM_SCREEN + $28, x
                        lda C_BITMAPSCREENPTR + $50, x   
                        sta MEM_SCREEN + $50, x
                        lda C_BITMAPSCREENPTR + $78, x   
                        sta MEM_SCREEN + $78, x
                        lda C_BITMAPSCREENPTR + $a0, x   
                        sta MEM_SCREEN + $a0, x
                        lda C_BITMAPSCREENPTR + $c8, x   
                        sta MEM_SCREEN + $c8, x
                        lda C_BITMAPSCREENPTR + $f0, x   
                        sta MEM_SCREEN + $f0, x
                        lda C_BITMAPSCREENPTR + $118, x   
                        sta MEM_SCREEN + $118, x

                        lda C_BITMAPCOLOURPTR, x
                        sta MEM_COLOURRAM, x 
                        lda C_BITMAPCOLOURPTR + $28, x
                        sta MEM_COLOURRAM + $28, x
                        lda C_BITMAPCOLOURPTR + $50, x
                        sta MEM_COLOURRAM + $50, x
                        lda C_BITMAPCOLOURPTR + $78, x
                        sta MEM_COLOURRAM + $78, x
                        lda C_BITMAPCOLOURPTR + $a0, x
                        sta MEM_COLOURRAM + $a0, x
                        lda C_BITMAPCOLOURPTR + $c8, x
                        sta MEM_COLOURRAM + $c8, x
                        lda C_BITMAPCOLOURPTR + $f0, x
                        sta MEM_COLOURRAM + $f0, x
                        lda C_BITMAPCOLOURPTR + $118, x
                        sta MEM_COLOURRAM + $118, x
                        inx
                        cpx #40 
                        bne init_bitmap

                        lda #00                 // current bitmap transition character row
                        sta REG_ZERO_2A

                        lda #<render_logo_start
                        sta set_postmusic_low + 1
                        lda #>render_logo_start
                        sta set_postmusic_high + 1
                        lda #49
                        sta set_postmusic_line + 1

                        lda #120
                        sta set_postlogo_end_line + 1
                        lda #<setup_main_demo_stars
                        sta set_postlogo_end_low + 1
                        lda #>setup_main_demo_stars
                        sta set_postlogo_end_high + 1

                        ldy #20
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


setup_main_demo_stars:  inc REG_INTFLAG         // acknowledge interrupt

                        ldx #00
setup_starfield:        lda C_STAR_CHARACTER_MAP, x
                        sta $0568, x
                        lda C_STAR_CHARACTER_MAP + $28, x
                        sta $0590, x
                        lda C_STAR_CHARACTER_MAP + $50, x
                        sta $05b8, x
                        lda C_STAR_CHARACTER_MAP + $78, x
                        sta $05e0, x
                        lda C_STAR_CHARACTER_MAP + $a0, x
                        sta $0608, x
                        lda C_STAR_CHARACTER_MAP + $c8, x
                        sta $0630, x
                        lda C_STAR_CHARACTER_MAP + $f0, x
                        sta $0658, x
                        lda C_STAR_CHARACTER_MAP + $118, x
                        sta $0680, x
                        lda C_STAR_CHARACTER_MAP + $140, x
                        sta $06a8, x
                        lda C_STAR_CHARACTER_MAP + $168, x
                        sta $06d0, x

                        lda #00                 // init text scroller screen & colour ram
                        sta MEM_COLOURRAM + $370, x
                        lda #160
                        sta MEM_SCREEN + $0370, x

                        inx
                        cpx #40
                        bne setup_starfield

                        // initialise snake characters in screen and colour ram
                        ldx #00
                        stx REG_ZERO_02
                        ldx #04
                        stx REG_ZERO_03
                        ldy #39
init_line_chars:        lda REG_ZERO_02
                        and #07
                        tax
                        lda linepattern, x
                        sta $0748, y
                        lda REG_ZERO_03
                        and #07
                        tax
                        lda linepattern2, x
                        inc REG_ZERO_03
                        sta $0798, y
                        inc REG_ZERO_02
                        dey
                        bpl init_line_chars

                        // update post music interrupt hook
                        lda #210
                        sta set_postlogo_end_line + 1
                        lda #<update_logo_transition
                        sta set_postlogo_end_low + 1
                        lda #>update_logo_transition
                        sta set_postlogo_end_high + 1

                        // ready to render bitmap transition
                        ldy #30
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // logo rendering ----------------------------------------------------]
                        // -------------------------------------------------------------------]

                        // set multicolour bitmap mode for logo
render_logo_start:      ldx #02                 // remove raster jitter
                        jsr raster_latch        

                        ldx logo_glow_index
                        lda logo_glow_colours, x
                        sta REG_BGCOLOUR

                        lda #59                 // enable bitmap mode
                        sta REG_SCREENCTL_1

                        lda #216                // enable multi colour mode
                        sta REG_SCREENCTL_2

                        lda #24                 // set bitmap data pointers
                        sta REG_MEMSETUP 

                        inc REG_INTFLAG         // acknowledge interrupt

set_postlogostart_line: ldy #114
                        sty REG_RASTERLINE
set_postlogostart_low:  lda #<render_logo_end
set_postlogostart_high: ldx #>render_logo_end
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // snake effect -------------------------------------------------------]
                        // --------------------------------------------------------------------]
update_scroller_snake:  inc REG_INTFLAG

                        dec snake_shift_delay
                        bne snakes_animate

                        lda #03
                        sta snake_shift_delay

                        ldx #00
shift_snake_chars_left: lda $0749, x
                        sta $0748, x
                        lda $0799, x
                        sta $0798, x
                        inx
                        cpx #39
                        bne shift_snake_chars_left
                        ldx linepattern_index
                        lda linepattern, x
                        sta $076f
                        lda linepattern2, x
                        sta $07bf

                        inx
                        txa
                        and #07
                        sta linepattern_index

snakes_animate:         ldx snakes_alive
                        cpx #01
                        beq shift_snake_colour_left - 2
                        dec snake_animate_delay
                        bne update_logo_flash_delay
                        lda #01
                        sta snakes_alive

                        ldx #00
shift_snake_colour_left: lda $db49, x
                        sta $db48, x
                        inx
                        cpx #39
                        bne shift_snake_colour_left
                        ldx snake_colour_index
                        lda snake_colours, x
                        sta $db6f

                        ldx #39
shift_snake_col_right:  lda $db97, x
                        sta $db98, x
                        dex
                        bne shift_snake_col_right
                        ldx snake_colour_index
                        lda snake_colours, x
                        sta $db98

                        clc
                        lda snake_colour_index
                        adc #01
                        and #31
                        sta snake_colour_index

update_logo_flash_delay: lda logo_flash_active
                        cmp #01
                        beq update_logo_flash

                        dec logo_flash_delay
                        bne snake_update_done
                        lda #01
                        sta logo_flash_active
                        jmp snake_update_done

update_logo_flash:      dec logo_flash_smooth
                        bne snake_update_done
                        lda #02
                        sta logo_flash_smooth
                        ldx logo_glow_index
                        inx
                        txa
                        and #15
                        sta logo_glow_index
                        cmp #00
                        bne snake_update_done
                        lda #00
                        sta logo_flash_active
                        lda #255
                        sta logo_flash_delay

snake_update_done:      ldy #114
                        sty REG_RASTERLINE
                        lda #<render_logo_end
                        ldx #>render_logo_end
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81

                        // restore rendering after logo
render_logo_end:        inc REG_INTFLAG         // acknowledge interrupt

                        lda #028                // set location of character set
                        sta REG_MEMSETUP

                        lda #27                 // disable bitmap mode
                        sta REG_SCREENCTL_1

                        lda #00
                        sta REG_SCREENCTL_2     // disable multi colour mode

                        lda #00
                        sta REG_BGCOLOUR

set_postlogo_end_line:  ldy #240
                        sty REG_RASTERLINE
set_postlogo_end_low:   lda #<update_logo_transition
set_postlogo_end_high:  ldx #>update_logo_transition
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81



                        // logo transition -------------------------------------------------]
                        // -----------------------------------------------------------------]

                        // update logo transition effect
update_logo_transition: inc REG_INTFLAG         // acknowledge interrupt

                        dec logo_transition_delay       // apply transition smoothing
                        beq continue_render
                        jmp fade_logo_complete

continue_render:        ldx #02                        // reset transition smoothing
                        stx logo_transition_delay

                        ldx logo_transition_index       // initialise offsets
                        stx REG_ZERO_02
                        ldx logo_transition_index_2
                        stx REG_ZERO_03

                        clc

                        ldy #19
transfer_logo_line:     ldx REG_ZERO_02                 // transfer even line
bitmap_copy_src1:       lda $4000, x
bitmap_copy_dest1:      sta $2000, x
bitmap_copy_src2:       lda $40a0, x
bitmap_copy_dest2:      sta $20a0, x

                        txa                             // advance to next horizontal byte
                        adc #08
                        sta REG_ZERO_02

                        ldx REG_ZERO_03                 // transfer odd line
bitmap_copy_src3:       lda $48c0, x
bitmap_copy_dest3:      sta $28c0, x
bitmap_copy_src4:       lda $4960, x
bitmap_copy_dest4:      sta $2960, x

                        txa                             // advance to next horizontal byte
                        adc #08
                        sta REG_ZERO_03
                        
                        dey
                        bpl transfer_logo_line

                        lda logo_transition_index
                        adc #02
                        cmp #08
                        beq advance_logo_char_row
                        jmp logo_index_updated

                        // advance to next logo character row
advance_logo_char_row:  lda #08
                        inc REG_ZERO_2A
                        cmp REG_ZERO_2A
                        bne advance_logo_pointers

                        // logo fade complete - update hook pointers
                        lda #<render_stars_top
                        ldx #>render_stars_top
                        sta set_postlogo_end_low + 1
                        stx set_postlogo_end_high + 1
                        lda #117
                        sta set_postlogo_end_line + 1

                        jmp fade_logo_complete


advance_logo_pointers:  lda REG_ZERO_2A
                        asl
                        tax
                        lda bitmap_render_src, x
                        sta bitmap_copy_src1 + 1
                        lda bitmap_render_src + 1, x
                        sta bitmap_copy_src1 + 2

                        lda bitmap_render_dest, x
                        sta bitmap_copy_dest1 + 1
                        lda bitmap_render_dest + 1, x
                        sta bitmap_copy_dest1 + 2

                        lda bitmap_render_src_2, x
                        sta bitmap_copy_src2 + 1
                        lda bitmap_render_src_2 + 1, x
                        sta bitmap_copy_src2 + 2

                        lda bitmap_render_dest_2, x
                        sta bitmap_copy_dest2 + 1
                        lda bitmap_render_dest_2 + 1, x
                        sta bitmap_copy_dest2 + 2

                        lda #08
                        sbc REG_ZERO_2A
                        sta REG_ZERO_52
                        ldx REG_ZERO_2A
                        stx REG_ZERO_2A
                        asl
                        tax
                        lda bitmap_render_src, x
                        sta bitmap_copy_src3 + 1
                        lda bitmap_render_src + 1, x
                        sta bitmap_copy_src3 + 2

                        lda bitmap_render_dest, x
                        sta bitmap_copy_dest3 + 1
                        lda bitmap_render_dest + 1, x
                        sta bitmap_copy_dest3 + 2

                        lda bitmap_render_src_2, x
                        sta bitmap_copy_src4 + 1
                        lda bitmap_render_src_2 + 1, x
                        sta bitmap_copy_src4 + 2

                        lda bitmap_render_dest_2, x
                        sta bitmap_copy_dest4 + 1
                        lda bitmap_render_dest_2 + 1, x
                        sta bitmap_copy_dest4 + 2

                        lda #00
logo_index_updated:     sta logo_transition_index
                        sta REG_ZERO_52
                        lda #08
                        sbc REG_ZERO_52
                        sta logo_transition_index_2

fade_logo_complete:      ldy #30
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // starfield transition -------------------------------------------]
                        // ----------------------------------------------------------------]

                        // tranition star field raster bars onto screen
update_bar_transition:   inc REG_INTFLAG         // acknowledge interrupt

                        dec raster_1_glow_adv   // apply smoothing to glow
                        bne rasters_fade_on_done
                        lda #03
                        sta raster_1_glow_adv

                        clc

                        lda raster_1_glow_index // advance colour index
                        adc #01
                        and #15
                        sta raster_1_glow_index
                        sta raster_2_glow_index
                        bne rasters_fade_on_done        // reached final fade in value?
                      
                        lda #<update_star_transition
                        ldx #>update_star_transition
                        sta set_post_starbot_low + 1
                        stx set_post_starbot_high + 1

                        lda #<star_raster_colours       // update colour table pointers
                        sta star_raster_colour_ptr1 + 1
                        sta star_raster_colour_ptr2 + 1

                        lda #>star_raster_colours
                        sta star_raster_colour_ptr1 + 2
                        sta star_raster_colour_ptr2 + 2

                        lda #80
                        sta raster_1_glow_adv
                        lda #88
                        sta raster_2_glow_adv

rasters_fade_on_done:    ldy #30
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81           



update_star_transition:  inc REG_INTFLAG         // acknowledge interrupt

                        dec starfield_delay
                        bne update_star_fade_delay
                        jsr scroll_star_field
        
update_star_fade_delay:  dec star_fade_delay
                        bne star_fade_update_done
                        lda #04
                        sta star_fade_delay

                        clc
                        ldy star_fadein_index
                        tya
                        asl
                        tax
                        lda star_colour_ram_ptr, x
                        sta copy_star_colour_clone + 1
                        lda star_colour_ram_ptr_2, x
                        sta copy_star_colour_clone2 + 1
                        inx
                        lda star_colour_ram_ptr, x
                        sta copy_star_colour_clone + 2
                        lda star_colour_ram_ptr_2, x
                        sta copy_star_colour_clone2 + 2

                        ldx #00
copy_star_colour_clone:  lda C_STAR_COLOUR_MAP_FINAL, x
                        sta $d968, x
copy_star_colour_clone2: lda C_STAR_COLOUR_MAP_FINAL, x
                        sta $da30, x
                        inx
                        cpx #200
                        bne copy_star_colour_clone

                        iny
                        cpy #007
                        bne star_fade_update_done - 3


                        lda #<render_text_scroller
                        ldx #>render_text_scroller
                        sta set_post_starbot_low + 1
                        stx set_post_starbot_high + 1
                        lda #226
                        sta set_post_starbot_line + 1
                        

                        lda #<update_scroller_snake
                        ldx #>update_scroller_snake
                        sta set_postlogostart_low + 1
                        stx set_postlogostart_high + 1
                        lda #55
                        sta set_postlogostart_line + 1

                        sty star_fadein_index
star_fade_update_done:  ldy #30
                        sty REG_RASTERLINE
                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81



                        // starfield rendering -----------------------------------------------]
                        // -------------------------------------------------------------------]
render_stars_top:       lda #028                // set location of character set
                        sta REG_MEMSETUP

                        lda #27                 // disable bitmap mode
                        sta REG_SCREENCTL_1

                        lda #$c8                 // disable multi colour mode
                        sta REG_SCREENCTL_2

                        ldx raster_1_glow_index // generate raster bar
star_raster_colour_ptr1: lda star_raster_fadein, x
                        sta REG_BORCOLOUR
                        sta REG_BGCOLOUR

                        inc REG_INTFLAG         // acknowledge interrupt

                        ldy #208
                        sty REG_RASTERLINE

                        lda #<render_stars_bottom
                        ldx #>render_stars_bottom
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        ldx #05                 // free cycles to reclaim later!
top_stars_raster_latch: dex
                        bne top_stars_raster_latch

                        lda #00                 // end raster bar
                        sta REG_BORCOLOUR
                        sta REG_BGCOLOUR

                        jmp $ea81

                        // lower raster bar on starfield
render_stars_bottom:    ldx #01
                        jsr raster_latch

                        ldx raster_2_glow_index
star_raster_colour_ptr2: lda star_raster_fadein, x
                        sta REG_BORCOLOUR
                        sta REG_BGCOLOUR

                        inc REG_INTFLAG         // acknowledge interrupt

set_post_starbot_line:  ldy #212
                        sty REG_RASTERLINE

set_post_starbot_low:   lda #<update_bar_transition     
set_post_starbot_high:  ldx #>update_bar_transition
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        ldx #04                 // free cycles to reclaim later!
bot_stars_raster_latch: dex
                        bne bot_stars_raster_latch
                        
                        lda #00                 // end raster bar
                        sta REG_BORCOLOUR
                        sta REG_BGCOLOUR

                        jmp $ea81


                        // text scoller -------------------------------------------------------]
                        // --------------------------------------------------------------------]

                        // render text scroller & raster bars
render_text_scroller:   inc REG_INTFLAG         // acknowledge interrupt
                        clc

                        lda REG_SCREENCTL_2
                        and #240                // set 38 column mode
                        adc scroll_magnitude    // apply scroll
                        sta REG_SCREENCTL_2

                        ldx raster_index_start  // apply first raster immediately
                        lda raster_colourtable, x      
                        sta REG_BGCOLOUR
                        inx

                        ldy #00
                        lda raster_latch_table, y
scroller_raster_latch:  sbc #01
                        bne scroller_raster_latch
                        lda raster_colourtable, x      // set background colour
                        sta REG_BGCOLOUR
                        inx
                        iny
                        cpy #08
                        bne scroller_raster_latch - 3

                        lda #00
                        sta REG_BGCOLOUR

                        lda REG_SCREENCTL_2     
                        and #240                // switch off scrolling
                        ora #08                 // switch to 40 column mode
                        sta REG_SCREENCTL_2

                        ldy #250                // trigger update in lower border
                        sty REG_RASTERLINE
                        lda #<demo_update       // load interrupt address
                        ldx #>demo_update
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


                        // main demo routines ---------------------------------------------]
                        // ----------------------------------------------------------------]

                        // demo update routine
demo_update:            inc REG_INTFLAG         // acknowledge interrupt

                        // scroll starfield
                        dec starfield_delay     
                        bne update_raster_cycle
                        jsr scroll_star_field
        
                        // text transition 
textcycle_routine_ptr:  jsr update_textcycle_waitof

                        // update scroller raster bar movement
update_raster_cycle:    clc
                        lda raster_cos_index    // cycle start colour
                        adc #01
                        and #127
                        sta raster_cos_index
                        cmp #63
                        bne set_next_raster_cos

                        clc
                        lda raster_colour_mod_index
                        adc #08
                        and #31
                        sta raster_colour_mod_index
                        tax

                        ldy #00
copy_next_raster_mod:   lda raster_colourtable_mod, x
                        sta raster_colourtable + $08, y
                        inx
                        iny
                        cpy #08
                        bne copy_next_raster_mod

set_next_raster_cos:    ldx raster_cos_index
                        lda C_COS_TABLE, x         // calculate next starting colour index
                        lsr
                        lsr
                        sta raster_index_start

                        // update hardware scroll 
update_scroller:        dec scroll_magnitude    
                        bpl scroller_update_done
                        lda #07                 
                        sta scroll_magnitude

                        ldx #00                 // shift characters in row
scroller_shift:         lda $0771, x
                        sta $0770, x
                        inx
                        cpx #39
                        bne scroller_shift

                        ldx scroller_char_index // grab next scroll test character
scroller_load_char:     lda C_MESSAGEPTR, x
                        sta $0797
                        inx
                        bne scroller_test_char
                        inc scroller_load_char + 2      // advance message start (256+ chars)
                        inc scroller_test_char + 2

scroller_test_char:     lda C_MESSAGEPTR, x
                        cmp #$ff                        // end of message?
                        bne scroller_update_done - 3
                        lda #>C_MESSAGEPTR
                        sta scroller_load_char + 2      // reset message start
                        sta scroller_test_char + 2
                        ldx #00
                        stx scroller_load_char + 1
                        stx scroller_test_char + 1
                        stx scroller_char_index

scroller_update_done:    lda REG_SCREENCTL_2     
                        and #240                // switch off scrolling
                        ora #08                 // switch to 40 column mode
                        sta REG_SCREENCTL_2

                        // update starfield raster glow
update_raster_glow:     dec raster_1_glow_adv
                        bne update_raster_glow_2

                        ldx #04
                        stx raster_1_glow_adv

                        clc

                        lda raster_1_glow_index
                        adc #01
                        tay
                        and #15
                        bne save_next_glow_1_index

                        ldx #255    // set crazy delay to allow glow 2 to catch up
                        stx raster_1_glow_adv

save_next_glow_1_index:  tya
                        and #63
                        sta raster_1_glow_index

update_raster_glow_2:   dec raster_2_glow_adv
                        bne update_complete

                        ldx #04
                        stx raster_2_glow_adv

                        clc

                        lda raster_2_glow_index
                        adc #01
                        tay
                        and #15
                        bne save_next_glow_2_index

                        ldx #60
                        stx raster_1_glow_adv
                        ldx #72
                        stx raster_2_glow_adv

save_next_glow_2_index:  tya
                        and #63
                        sta raster_2_glow_index

                        // set up next interrupt
update_complete:        ldy #30
                        sty REG_RASTERLINE

                        lda #<update_music_player
                        ldx #>update_music_player
                        sta REG_INTSERVICE_LOW
                        stx REG_INTSERVICE_HIGH

                        jmp $ea81


scroll_star_field:      lda #C_STAR_DELAY        // reset timer
                        sta starfield_delay

                        ldy #03
                        ldx #01
roll_star_set_1:        lda C_CHARSET_STARS + $20, x
                        ror
                        ror C_CHARSET_STARS, x
                        ror C_CHARSET_STARS + $08, x
                        ror C_CHARSET_STARS + $10, x
                        ror C_CHARSET_STARS + $18, x
                        ror C_CHARSET_STARS + $20, x
                        dey
                        bpl roll_star_set_1

                        ldy #04
                        ldx #03
roll_star_set_2:        lda C_CHARSET_STARS + $48, x
                        ror         
                        ror C_CHARSET_STARS + $28, x
                        ror C_CHARSET_STARS + $30, x
                        ror C_CHARSET_STARS + $38, x
                        ror C_CHARSET_STARS + $40, x
                        ror C_CHARSET_STARS + $48, x
                        dey
                        bpl roll_star_set_2

                        ldy #02
                        ldx #04
roll_star_set_3:        lda C_CHARSET_STARS + $70, x
                        ror
                        ror C_CHARSET_STARS + $50, x
                        ror C_CHARSET_STARS + $58, x
                        ror C_CHARSET_STARS + $60, x
                        ror C_CHARSET_STARS + $68, x
                        ror C_CHARSET_STARS + $70, x
                        dey
                        bpl roll_star_set_3

                        ldx #7
                        lda C_CHARSET_STARS + $20, x
                        ror
                        ror C_CHARSET_STARS, x
                        ror C_CHARSET_STARS + $08, x
                        ror C_CHARSET_STARS + $10, x
                        ror C_CHARSET_STARS + $18, x
                        ror C_CHARSET_STARS + $20, x
                        rts

raster_latch:           dex 
                        bne raster_latch
                        rts


                        // text cycle - cycle on ----------------------------------------]
                        // --------------------------------------------------------------]

                        // cycle text onto screen
update_textcycle_on:    dec textcycle_delay             // apply smoothing delay
                        beq update_textcycle_ok
                        rts

update_textcycle_ok:    lda #02                         // reset delay
                        sta textcycle_delay

                        ldy textcycle_char_index        // get next character index
                        cpy #18                         // reached maximum string length?
                        bcs update_cycle_on_colours

                        clc
                        tya
                        adc message_index_offset        // add offset into message block
                        tax

                        lda C_TEXT_CYCLE_MSGS, x        // display character in message 1
                        cmp #32
                        beq second_message_on
                        sta $05c4, y

second_message_on:      lda C_TEXT_CYCLE_MSGS + $b4, x  // display character in message 2
                        cmp #32
                        beq third_message_on
                        sta $0614, y

third_message_on:       lda C_TEXT_CYCLE_MSGS + $168, x // display character in message 3
                        cmp #32
                        beq textcycle_on_adv_char
                        sta $0664, y


textcycle_on_adv_char:  lda textcycle_char_index
                        cmp #18
                        bcs update_cycle_on_colours
                        inc textcycle_char_index



update_cycle_on_colours: ldy textcycle_colour_index      // get the first colour index
                        sty REG_ZERO_02

                        clc

                        lda message_index_offset
                        adc textcycle_char_index
                        sta REG_ZERO_2A

                        lda message_index_offset
                        adc textcycle_start_offset
                        tax

                        ldy textcycle_start_offset
                        sty REG_ZERO_52

update_cycle_on_loop:   ldy REG_ZERO_02
                        lda textcycle_colours_in, y     // get colour to write

                        ldy C_TEXT_CYCLE_MSGS, x        // test character
                        cpy #32
                        beq second_message_c_on
                        ldy REG_ZERO_52
                        sta $d9c4, y

second_message_c_on:    ldy C_TEXT_CYCLE_MSGS + $b4, x
                        cpy #32
                        beq third_message_c_on
                        ldy REG_ZERO_52
                        sta $da14, y

third_message_c_on:     ldy C_TEXT_CYCLE_MSGS + $168, x
                        cpy #32
                        beq all_messages_done
                        ldy REG_ZERO_52
                        sta $da64, y

all_messages_done:      dec REG_ZERO_02
                        inc REG_ZERO_52
                        inx
                        cpx REG_ZERO_2A
                        bcc update_cycle_on_loop

                        lda textcycle_colour_index
                        cmp #11
                        bcs advance_textcycle_start
                        inc textcycle_colour_index
                        jmp update_cycle_srt_indx
advance_textcycle_start: inc textcycle_start_offset

update_cycle_srt_indx:  lda textcycle_start_offset
                        cmp #18
                        bcc update_cycle_on_done

update_cycle_advance:   lda #48
                        sta textcycle_delay
                        lda #<update_textcycle_waiton
                        ldx #>update_textcycle_waiton 
                        sta textcycle_routine_ptr + 1
                        stx textcycle_routine_ptr + 2

                        lda #03
                        sta REG_ZERO_03

update_cycle_on_done:   rts


                        // text cycle - cycle off ------------------------------------------]
                        // -----------------------------------------------------------------]

                        // cycle text off screen
update_textcycle_off:   dec textcycle_delay             // apply smoothing delay
                        beq update_textcycle_off_ok
                        rts

update_textcycle_off_ok: lda #02                         // reset delay
                        sta textcycle_delay

                        ldy textcycle_char_index        // ready to reset a character from cache?
                        cpy #07
                        bcc update_cycle_off_cols

                        clc                             // determine message offset
                        lda textcycle_start_offset
                        tay
                        adc message_index_offset
                        tax

                        lda C_TEXT_CYCLE_MSGS, x
                        cmp #32
                        beq second_message_off
                        lda C_STAR_CHARACTER_MAP + $5c, y
                        sta $05c4, y
                        lda C_STAR_COLOUR_MAP_FINAL + $5c, y
                        sta $d9c4, y

second_message_off:     lda C_TEXT_CYCLE_MSGS + $b4, x
                        cmp #32
                        beq third_message_off
                        lda C_STAR_CHARACTER_MAP + $ac, y
                        sta $0614, y
                        lda C_STAR_COLOUR_MAP_FINAL + $ac, y
                        sta $da14, y

third_message_off:      lda C_TEXT_CYCLE_MSGS + $168, x
                        cmp #32
                        beq update_cycle_off_index
                        lda C_STAR_CHARACTER_MAP + $fc, y
                        sta $0664, y
                        lda C_STAR_COLOUR_MAP_FINAL + $fc, y
                        sta $da64, y

update_cycle_off_index: inc textcycle_start_offset
                        
                        ldx textcycle_start_offset
                        cpx #18
                        bne update_cycle_off_cols

                        lda #48
                        sta textcycle_delay
                        lda #<update_textcycle_waitof
                        ldx #>update_textcycle_waitof 
                        sta textcycle_routine_ptr + 1
                        stx textcycle_routine_ptr + 2
                        
                        lda #04
                        sta REG_ZERO_03

                        clc
                        lda message_index_offset
                        adc #18
                        cmp #180
                        bne advance_message_index
                        lda #00

advance_message_index:  sta message_index_offset
                        jmp update_cycle_off_done

update_cycle_off_cols:  ldy textcycle_colour_index
                        sty REG_ZERO_02

                        clc

                        lda message_index_offset
                        adc textcycle_char_index
                        sta REG_ZERO_2A

                        clc

                        lda message_index_offset
                        adc textcycle_start_offset
                        tax

                        ldy textcycle_start_offset
                        sty REG_ZERO_52

update_cycle_off_loop:  ldy REG_ZERO_02
                        lda textcycle_colours_out, y     // get colour to write

                        ldy C_TEXT_CYCLE_MSGS, x         // test character
                        cpy #32
                        beq second_message_c_off
                        ldy REG_ZERO_52
                        sta $d9c4, y

second_message_c_off:   ldy C_TEXT_CYCLE_MSGS + $b4, x
                        cpy #32
                        beq third_message_c_off
                        ldy REG_ZERO_52
                        sta $da14, y

third_message_c_off:    ldy C_TEXT_CYCLE_MSGS + $168, x
                        cpy #32
                        beq saaa
                        ldy REG_ZERO_52
                        sta $da64, y

saaa:                   dec REG_ZERO_02
                        inc REG_ZERO_52
                        inx
                        cpx REG_ZERO_2A
                        bcc update_cycle_off_loop

update_cycle_char_indx: lda textcycle_char_index
                        cmp #18
                        bcs update_cycle_off_indx
                        inc textcycle_char_index

update_cycle_off_indx:  lda textcycle_colour_index
                        cmp #07
                        bcs update_cycle_off_done
                        inc textcycle_colour_index

update_cycle_off_done:  rts


                        // text cycle wait states ------------------------------------------]
                        // -----------------------------------------------------------------]

                        // update delay until displayed text ready to be removed
update_textcycle_waiton: dec textcycle_delay
                        bne update_waiton_complete

                        lda #01
                        sta textcycle_delay

                        lda #00
                        sta textcycle_char_index
                        sta textcycle_colour_index
                        sta textcycle_start_offset

                        lda #<update_textcycle_off
                        ldx #>update_textcycle_off 
                        sta textcycle_routine_ptr + 1
                        stx textcycle_routine_ptr + 2

                        lda #02
                        sta REG_ZERO_03

update_waiton_complete: rts

                        // update delay until next text section ready
update_textcycle_waitof: dec textcycle_delay
                        bne update_waitoff_complete

                        lda #01
                        sta textcycle_delay

                        lda #00
                        sta textcycle_char_index
                        sta textcycle_colour_index
                        sta textcycle_start_offset

                        lda #<update_textcycle_on
                        ldx #>update_textcycle_on 
                        sta textcycle_routine_ptr + 1
                        stx textcycle_routine_ptr + 2

                        lda #01
                        sta REG_ZERO_03

update_waitoff_complete: rts



 // ########################################################################################]
 // ####
 // #### demo variables & lookup tables
 // ####
 // ########################################################################################]
                        //  ----------------------------------------------------------------]

                        // logo transition pointers & variables ----------------------------]
                        // -----------------------------------------------------------------]
bitmap_render_src:      .byte $00, $40, $40, $41, $80, $42, $c0, $43
                        .byte $00, $45, $40, $46, $80, $47, $c0, $48
bitmap_render_src_2:    .byte $a0, $40, $e0, $41, $20, $43, $60, $44
                        .byte $a0, $45, $e0, $46, $20, $48, $60, $49
bitmap_render_dest:     .byte $00, $20, $40, $21, $80, $22, $c0, $23
                        .byte $00, $25, $40, $26, $80, $27, $c0, $28
bitmap_render_dest_2:   .byte $a0, $20, $e0, $21, $20, $23, $60, $24
                        .byte $a0, $25, $e0, $26, $20, $28, $60, $29

logo_transition_delay:  .byte 002
complete_bitmap_rows:   .byte 000
logo_transition_index:  .byte 000
logo_transition_index_2: .byte 007

logo_glow_colours:      .byte 000, 011, 012, 011, 000, 015, 001, 015
                        .byte 012, 011, 000, 000, 000, 000, 000, 000
logo_glow_index:        .byte 000
logo_flash_active:      .byte 000
logo_flash_delay:       .byte 200
logo_flash_smooth:      .byte 002

                        // starfield data tables & variables --------------------------------]
                        // ------------------------------------------------------------------]
starfield_delay:        .byte 002
raster_1_glow_index:    .byte 000
raster_2_glow_index:    .byte 000
raster_1_glow_adv:      .byte 001
raster_2_glow_adv:      .byte 001

star_raster_fadein:  .byte 000, 000, 000, 011, 012, 015, 001, 015, 012, 011, 012, 015, 001, 001, 015, 015
star_raster_colours: .byte 015, 007, 010, 002, 009, 002, 010, 007, 010, 002, 009, 002, 010, 007, 001, 015
                     .byte 015, 003, 014, 004, 006, 004, 014, 003, 014, 004, 006, 004, 014, 003, 001, 015
                     .byte 015, 013, 005, 008, 009, 008, 005, 013, 005, 008, 009, 008, 005, 013, 001, 015
                     .byte 015, 001, 015, 012, 011, 012, 015, 001, 015, 012, 011, 012, 015, 001, 015, 015

star_colour_ram_ptr:    .byte $00, $50, $90, $51, $20, $53, $b0, $54, $40, $56, $d0, $57, $60, $59
star_colour_ram_ptr_2:  .byte $c8, $50, $58, $52, $e8, $53, $78, $55, $08, $57, $98, $58, $28, $5a
star_fadein_index:      .byte 000
star_fade_delay:        .byte 002

                        // text scroll raster bar data & variables ----------------------------]
                        // --------------------------------------------------------------------]
scroll_magnitude:       .byte 007               // start at 7 for left scroll
scroller_char_index:    .byte 000               // message character index

raster_cos_index:       .byte 000
raster_index_start:     .byte 000
raster_latch_table:     .byte 003, 007, 010, 009, 009, 010, 009, 009, 009

raster_colourtable:     .byte 011, 012, 015, 001, 001, 015, 012, 011
                        .byte 006, 004, 014, 003, 003, 014, 004, 006
                        .byte 011, 012, 015, 001, 001, 015, 012, 011

raster_colourtable_mod: .byte 006, 004, 014, 003, 003, 014, 004, 006
                        .byte 006, 014, 003, 001, 001, 003, 014, 006
                        .byte 009, 008, 005, 013, 013, 005, 008, 009
                        .byte 015, 003, 013, 001, 001, 013, 003, 015
raster_colour_mod_index: .byte 000

                        // intro text variables & tables -------------------------------------]
                        // -------------------------------------------------------------------]
intro_message_index:    .byte 000
left_scroll_magnitude:  .byte 007
right_scroll_magnitude: .byte 000

scroll_delay:           .byte 002
cycle_delay:            .byte 002
intro_msg_timer:        .byte 000
intro_text_col_index:   .byte 000

intro_text_colours_in:  .byte 006, 004, 014, 003, 001, 015, 012, 012
                        .byte 012, 012, 012, 012, 015, 001, 015, 012
intro_text_colours_out: .byte 012, 015, 001, 003, 014, 004, 006, 000
                        .byte 000, 000, 000, 000, 000, 000, 000, 000

scroll_right_start:     .byte 066, 210, 106, 170, 210, 138
scroll_right_end:       .byte 074, 218, 114, 178, 218, 146
scroll_left_start:      .byte 082, 226, 122, 186, 226, 154
scroll_left_end:        .byte 090, 234, 130, 194, 234, 162

left_message_scrram:    .byte $a0, $04, $70, $07, $68, $05, $a8, $06, $70, $07, $08, $06
right_message_scrram:   .byte $50, $04, $20, $07, $18, $05, $58, $06, $20, $07, $b8, $05
left_message_colram:    .byte $a0, $d8, $70, $db, $68, $d9, $a8, $da, $70, $db, $08, $da
right_message_colram:   .byte $50, $d8, $20, $db, $18, $d9, $58, $da, $20, $db, $b8, $d9

left_messages:          .byte <l_message_1, >l_message_1, <l_message_2, >l_message_2 
                        .byte <l_message_3, >l_message_3, <l_message_4, >l_message_4
                        .byte <l_message_5, >l_message_5, <l_message_6, >l_message_6
right_messages:         .byte <r_message_1, >r_message_1, <r_message_2, >r_message_2
                        .byte <r_message_3, >r_message_3, <r_message_4, >r_message_4
                        .byte <r_message_5, >r_message_5, <r_message_6, >r_message_6

intro_messages:
r_message_1:            .text "  presenting to you                     "
l_message_1:            .text "                         a small intro  "
r_message_2:            .text "   my very first                        "
l_message_2:            .text "                    ** c64 release **   "
r_message_3:            .text "  old skool inspired                    "
l_message_3:            .text "               (..ghetto rasters ahead) "
r_message_4:            .text "    first released                      "
l_message_4:            .text "                       at syntax#14     "
r_message_5:            .text " australia's premier                    "
l_message_5:            .text "                   demo scene party     "
r_message_6:            .text "    simply titled                      "
l_message_6:            .text "                          ********      "


                        // intro transition variables & tables ---------------------------]
                        // ---------------------------------------------------------------]
bgcolour_cache:         .byte 000
intro_glow_delay:       .byte 002
intro_glow_index:       .byte 000
intro_glow_colours:     .byte 000, 015, 007, 010, 002, 009, 002, 010
                        .byte 007, 010, 002, 009, 002, 010, 007, 001
                        .byte 015, 012, 011, 012, 015, 001, 015, 012
                        .byte 011, 000
intro_back_glow_colours: .byte 000, 012, 006, 004, 014, 003, 001, 003
                        .byte 014, 004, 006, 011, 002, 008, 010, 007
                        .byte 007, 010, 008, 002, 012, 011, 000, 000

cell_glow_colours:      .byte 012, 015, 001, 007, 010, 002, 009, 000
sequence_advance_delay: .byte 065
sequence_glow_delay:    .byte 003
next_sequence_index:    .byte 000
sequence_cell_list_ptr: .byte 000
sequence_cell_list:     .byte 001, 000
                        .byte 002, 010, 001
                        .byte 003, 020, 011, 002
                        .byte 004, 030, 021, 012, 003
                        .byte 005, 040, 031, 022, 013, 004
                        .byte 006, 050, 041, 032, 023, 014, 005
                        .byte 006, 051, 042, 033, 024, 015, 006
                        .byte 006, 052, 043, 034, 025, 016, 007
                        .byte 006, 053, 044, 035, 026, 017, 008
                        .byte 006, 054, 045, 036, 027, 018, 009
                        .byte 005, 055, 046, 037, 028, 019
                        .byte 004, 056, 047, 038, 029
                        .byte 003, 057, 048, 039
                        .byte 002, 058, 049
                        .byte 001, 059
active_sequence_count:  .byte 000
first_active_sequence:  .byte 000
newly_active_cell_count: .byte 000
newly_active_cell_list: .byte 000, 000, 000, 000, 000, 000
sequence_colour_index:  .byte 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000

sequence_update_ptr:    .byte <update_sequence_0, >update_sequence_0
                        .byte <update_sequence_1, >update_sequence_1
                        .byte <update_sequence_2, >update_sequence_2
                        .byte <update_sequence_3, >update_sequence_3
                        .byte <update_sequence_4, >update_sequence_4
                        .byte <update_sequence_5, >update_sequence_5
                        .byte <update_sequence_6, >update_sequence_6
                        .byte <update_sequence_7, >update_sequence_7
                        .byte <update_sequence_8, >update_sequence_8
                        .byte <update_sequence_9, >update_sequence_9
                        .byte <update_sequence_10, >update_sequence_10
                        .byte <update_sequence_11, >update_sequence_11
                        .byte <update_sequence_12, >update_sequence_12
                        .byte <update_sequence_13, >update_sequence_13
                        .byte <update_sequence_14, >update_sequence_14

custom_intro_chars:     .byte 000, 127, 127, 127, 127, 127, 127, 127
                        .byte 000, 254, 254, 254, 254, 254, 254, 254
                        .byte 127, 127, 127, 127, 127, 127, 127, 000
                        .byte 254, 254, 254, 254, 254, 254, 254, 000
                        .byte 000, 255, 255, 255, 255, 255, 255, 255
                        .byte 127, 127, 127, 127, 127, 127, 127, 127
                        .byte 254, 254, 254, 254, 254, 254, 254, 254
                        .byte 255, 255, 255, 255, 255, 255, 255, 000

cell_char_index:        .byte 000
intro_block_def:        .byte 192, 196, 196, 193
                        .byte 197, 160, 160, 198
                        .byte 197, 160, 160, 198
                        .byte 194, 199, 199, 195

linepattern:            .byte 079, 080, 080, 080, 079, 081, 081, 081
linepattern2:           .byte 082, 084, 084, 084, 082, 083, 083, 083
linepattern_index:      .byte 000
snake_shift_delay:      .byte 003
snake_animate_delay:    .byte 018
snake_colour_index:     .byte 000
snakes_alive:           .byte 000
snake_colours:          .byte 000, 002, 008, 010, 007, 010, 008, 002
                        .byte 000, 000, 000, 000, 000, 000, 000, 000
                        .byte 006, 004, 014, 003, 003, 014, 004, 006
                        .byte 000, 000, 000, 000, 000, 000, 000, 000


                        // text cycle variables & tables --------------------------------]
                        // --------------------------------------------------------------]
init_colour_table:      .byte 001, 002, 003, 004, 005, 009, 007, 008

textcycle_delay:        .byte 008
textcycle_colour_index: .byte 000
textcycle_char_index:   .byte 000
textcycle_start_offset: .byte 000
message_index_offset:   .byte 000
textcycle_colours_in:   .byte 002, 008, 010, 007, 001, 015, 012, 012, 012, 015, 001, 015, 012
textcycle_colours_out:  .byte 012, 015, 001, 007, 010, 008, 002, 000

                        // logo bitmap data (rle packed 2933 bytes) -------------------]
                        // ------------------------------------------------------------]
bitmap_data:   .byte $aa, $aa, $00, $aa, $9a, $a6, $9a, $a6, $aa, $aa, $00, $aa, $a6, $a9, $66, $99
               .byte $aa, $aa, $02, $9a, $a6, $9a, $a6, $9a, $aa, $aa, $22, $a9, $a6, $a9, $a6, $99
               .byte $aa, $aa, $a8, $a9, $aa, $a9, $6a, $a9, $aa, $aa, $80, $aa, $68, $a8, $60, $a0
               .byte $80, $80, $02, $09, $26, $29, $a6, $99, $00, $2a, $aa, $aa, $a9, $a6, $a9, $a6
               .byte $00, $80, $68, $9a, $56, $55, $75, $5d, $aa, $2a, $00, $02, $82, $80, $60, $60
               .byte $ff, $ff, $00, $75, $d5, $59, $56, $55, $ff, $ff, $0c, $57, $dd, $55, $65, $59
               .byte $ff, $77, $cd, $75, $55, $96, $59, $96, $a1, $61, $a1, $51, $52, $52, $f2, $58
               .byte $c2, $03, $82, $c3, $92, $c2, $03, $d3, $00, $00, $40, $40, $43, $40, $80, $80
               .byte $c0, $00, $0b, $00, $00, $08, $00, $00, $80, $c0, $f8, $c0, $80, $c2, $07, $00
               .byte $01, $03, $1f, $03, $c2, $06, $00, $d0, $c2, $21, $00, $c2, $03, $01, $03, $09
               .byte $0b, $0b, $07, $c2, $03, $85, $8d, $85, $0e, $0e, $2e, $15, $16, $19, $2a, $2e
               .byte $2a, $3b, $2a, $55, $66, $99, $a6, $aa, $eb, $ba, $eb, $55, $66, $99, $6a, $99
               .byte $aa, $ae, $ba, $55, $66, $99, $a6, $a9, $ba, $ea, $aa, $aa, $99, $66, $95, $59
               .byte $95, $55, $55, $aa, $9a, $65, $59, $65, $95, $56, $55, $aa, $aa, $a2, $59, $65
               .byte $96, $55, $65, $aa, $aa, $20, $9a, $95, $56, $65, $55, $aa, $aa, $00, $aa, $aa
               .byte $66, $99, $66, $aa, $aa, $00, $c2, $03, $aa, $9a, $65, $aa, $aa, $00, $aa, $a9
               .byte $aa, $a9, $9a, $aa, $aa, $00, $aa, $aa, $6a, $a6, $69, $aa, $aa, $00, $a9, $6a
               .byte $99, $6a, $99, $aa, $aa, $00, $aa, $6a, $aa, $6a, $a9, $99, $66, $59, $65, $59
               .byte $65, $59, $56, $56, $59, $96, $65, $56, $c2, $03, $55, $66, $99, $55, $96, $59
               .byte $96, $65, $95, $65, $95, $65, $59, $65, $59, $95, $55, $66, $99, $66, $59, $65
               .byte $59, $65, $95, $e2, $82, $c2, $01, $c2, $c2, $05, $c9, $55, $76, $d8, $78, $e0
               .byte $60, $e0, $60, $aa, $02, $c2, $03, $00, $20, $98, $98, $57, $9f, $95, $a7, $25
               .byte $25, $27, $25, $58, $d8, $d6, $c2, $04, $f6, $f8, $19, $16, $19, $16, $19, $15
               .byte $15, $00, $65, $59, $a5, $66, $99, $6a, $55, $00, $65, $96, $59, $66, $99, $aa
               .byte $55, $00, $58, $58, $d4, $54, $fc, $dc, $7c, $00, $d2, $a1, $a1, $e1, $c2, $03
               .byte $a0, $00, $c0, $d0, $d0, $c2, $04, $90, $00, $02, $30, $00, $00, $04, $00, $40
               .byte $c2, $03, $00, $30, $00, $00, $80, $03, $00, $43, $00, $00, $01, $00, $00, $08
               .byte $c2, $04, $00, $01, $00, $20, $c2, $08, $00, $10, $c2, $0e, $00, $04, $c2, $04
               .byte $00, $03, $c0, $00, $20, $00, $01, $0d, $0d, $2e, $26, $26, $c2, $03, $24, $2e
               .byte $24, $24, $1c, $c2, $04, $14, $2a, $e6, $e9, $e6, $59, $65, $59, $aa, $a6, $69
               .byte $9a, $66, $99, $55, $55, $aa, $a6, $9a, $a5, $66, $99, $55, $55, $aa, $9a, $69
               .byte $9a, $69, $96, $55, $55, $aa, $65, $55, $99, $a6, $99, $aa, $aa, $55, $55, $99
               .byte $55, $66, $99, $aa, $aa, $c2, $03, $55, $99, $66, $99, $aa, $aa, $c2, $03, $55
               .byte $96, $65, $9a, $a9, $aa, $c2, $04, $55, $99, $66, $99, $aa, $55, $69, $9a, $aa
               .byte $aa, $ba, $ea, $fe, $aa, $69, $96, $65, $96, $55, $56, $d5, $55, $95, $65, $99
               .byte $56, $95, $55, $95, $65, $66, $99, $65, $59, $65, $59, $56, $59, $66, $99, $56
               .byte $65, $96, $65, $59, $c2, $07, $55, $65, $c2, $05, $55, $56, $59, $56, $55, $75
               .byte $57, $c2, $03, $55, $95, $55, $59, $55, $55, $57, $55, $55, $65, $56, $65, $75
               .byte $d5, $55, $55, $59, $55, $55, $59, $c2, $08, $c9, $c2, $04, $e0, $f8, $d8, $78
               .byte $de, $96, $95, $25, $09, $02, $c2, $03, $00, $97, $97, $5f, $7d, $95, $2a, $00
               .byte $00, $f8, $60, $60, $80, $80, $c2, $04, $00, $aa, $96, $96, $9e, $9f, $27, $25
               .byte $c2, $03, $02, $09, $09, $8b, $8b, $ed, $aa, $77, $dd, $77, $dd, $57, $5d, $d5
               .byte $aa, $fe, $f6, $78, $d8, $78, $58, $d8, $00, $0a, $0b, $c2, $03, $09, $25, $26
               .byte $00, $aa, $7f, $ff, $77, $5d, $77, $5d, $00, $aa, $fe, $fe, $77, $dd, $77, $5d
               .byte $00, $10, $03, $00, $80, $81, $80, $80, $00, $c2, $04, $02, $c2, $03, $09, $00
               .byte $a8, $58, $58, $d8, $78, $f8, $60, $00, $3f, $39, $39, $3a, $39, $3a, $e9, $00
               .byte $ff, $c2, $03, $55, $95, $55, $95, $00, $ff, $5b, $67, $5b, $67, $9e, $6e, $00
               .byte $00, $0c, $00, $02, $03, $2f, $03, $c2, $03, $00, $20, $00, $00, $d1, $00, $00
               .byte $02, $02, $c2, $01, $c2, $0a, $02, $09, $09, $00, $aa, $dd, $75, $d6, $55, $66
               .byte $59, $00, $aa, $65, $99, $65, $95, $57, $5d, $00, $aa, $7f, $c2, $03, $ff, $77
               .byte $dd, $00, $aa, $c2, $04, $ff, $77, $de, $00, $c2, $05, $80, $82, $02, $00, $aa
               .byte $c2, $04, $bf, $7f, $dd, $00, $aa, $dd, $f7, $fd, $ff, $ff, $d7, $00, $aa, $56
               .byte $59, $56, $55, $75, $dd, $00, $aa, $a9, $aa, $6a, $9a, $56, $55, $00, $aa, $95
               .byte $65, $99, $a6, $a9, $a6, $05, $c2, $05, $85, $87, $85, $c2, $08, $55, $57, $c2
               .byte $03, $55, $65, $99, $65, $55, $55, $75, $c2, $03, $55, $56, $55, $96, $55, $56
               .byte $65, $59, $55, $65, $59, $e6, $65, $55, $56, $95, $66, $95, $66, $95, $56, $55
               .byte $65, $59, $55, $65, $59, $6a, $55, $55, $56, $95, $66, $95, $66, $95, $95, $55
               .byte $59, $66, $59, $95, $59, $66, $42, $62, $62, $50, $64, $54, $d5, $5d, $76, $5d
               .byte $77, $95, $99, $26, $0a, $08, $00, $aa, $55, $55, $99, $66, $aa, $08, $00, $00
               .byte $a0, $5a, $55, $65, $aa, $88, $c2, $04, $00, $80, $80, $a0, $a0, $09, $09, $c2
               .byte $03, $02, $c2, $03, $00, $fd, $75, $56, $65, $99, $a6, $99, $aa, $55, $59, $66
               .byte $59, $95, $66, $99, $aa, $58, $58, $c2, $04, $60, $a0, $a0, $29, $26, $29, $26
               .byte $29, $26, $a9, $aa, $57, $5d, $65, $55, $99, $66, $99, $aa, $7f, $5d, $55, $65
               .byte $96, $65, $9a, $a9, $c2, $05, $60, $58, $68, $58, $c2, $05, $09, $25, $29, $25
               .byte $c2, $04, $e0, $60, $e2, $62, $a2, $ea, $da, $e6, $d9, $e6, $d9, $d6, $c5, $ae
               .byte $bb, $ae, $ab, $66, $99, $66, $99, $ad, $ad, $c2, $03, $ac, $9f, $66, $99, $03
               .byte $c2, $04, $00, $ff, $67, $97, $00, $0c, $82, $00, $04, $c2, $03, $00, $09, $09
               .byte $c2, $04, $0a, $2a, $0a, $66, $99, $c2, $03, $a9, $a5, $99, $65, $56, $59, $65
               .byte $59, $65, $57, $95, $57, $55, $56, $59, $aa, $c2, $04, $80, $56, $66, $9a, $aa
               .byte $c2, $04, $00, $82, $c2, $04, $02, $0a, $0a, $09, $66, $59, $66, $59, $d6, $75
               .byte $dd, $f7, $65, $56, $75, $7d, $5f, $97, $5f, $77, $f7, $7d, $5f, $aa, $c2, $04
               .byte $80, $75, $dd, $75, $aa, $c2, $03, $02, $09, $59, $66, $59, $d6, $7a, $d6, $7e
               .byte $de, $85, $85, $87, $85, $05, $07, $05, $0f, $56, $65, $55, $99, $56, $59, $96
               .byte $69, $55, $55, $66, $55, $65, $56, $65, $56, $65, $55, $55, $99, $56, $59, $96
               .byte $a9, $1e, $07, $01, $c2, $05, $00, $fe, $ff, $7f, $15, $c2, $03, $00, $55, $af
               .byte $bf, $ff, $ff, $c2, $03, $00, $55, $dd, $f7, $ff, $ff, $c2, $03, $00, $a1, $99
               .byte $66, $aa, $aa, $c2, $03, $00, $59, $96, $69, $9a, $aa, $c2, $03, $00, $af, $02
               .byte $c0, $70, $fc, $c2, $03, $00, $f7, $00, $a0, $0a, $c2, $04, $00, $54, $22, $08
               .byte $00, $80, $c2, $03, $00, $05, $28, $88, $08, $08, $c2, $03, $00, $55, $00, $00
               .byte $02, $02, $c2, $03, $00, $df, $88, $a2, $c2, $05, $00, $d7, $88, $22, $88, $c2
               .byte $04, $00, $75, $a0, $c2, $03, $80, $c2, $03, $00, $55, $88, $a2, $88, $a2, $80
               .byte $00, $00, $df, $88, $22, $88, $c2, $04, $00, $5f, $8a, $2a, $0a, $0a, $02, $00
               .byte $00, $50, $58, $78, $76, $5a, $56, $9e, $9d, $95, $25, $25, $27, $25, $27, $95
               .byte $a7, $97, $e1, $c2, $03, $81, $c2, $03, $80, $aa, $f3, $0c, $00, $55, $c2, $03
               .byte $00, $aa, $ff, $33, $00, $55, $c2, $03, $00, $aa, $ff, $ff, $33, $55, $c2, $03
               .byte $00, $80, $5f, $5d, $5c, $fc, $c2, $04, $00, $08, $c2, $06, $00, $15, $2a, $22
               .byte $28, $22, $08, $00, $00, $55, $aa, $22, $88, $c2, $04, $00, $55, $aa, $22, $82
               .byte $02, $c2, $03, $00, $55, $80, $c2, $06, $00, $05, $c2, $07, $00, $9a, $c2, $03
               .byte $0a, $08, $c2, $03, $00, $ff, $eb, $aa, $22, $88, $c2, $03, $00, $55, $ba, $aa
               .byte $22, $82, $c2, $03, $00, $55, $80, $c2, $06, $00, $55, $0a, $08, $0a, $08, $c2
               .byte $03, $00, $55, $aa, $8a, $22, $02, $c2, $03, $00, $54, $c2, $04, $0f, $c2, $03
               .byte $00, $15, $9a, $6a, $aa, $aa, $c2, $03, $00, $ff, $95, $a6, $aa, $ff, $c2, $03
               .byte $00, $ff, $a9, $a4, $50, $c2, $05, $00, $0f, $39, $e6, $59, $95, $65, $56, $59
               .byte $aa, $99, $66, $95, $59, $65, $55, $96, $aa, $99, $66, $55, $96, $65, $56, $55
               .byte $f1, $e1, $b1, $e1, $a3, $e3, $a1, $a1, $c2, $08, $00, $08, $0c, $0c, $04, $0c
               .byte $04, $04, $01, $c2, $03, $00, $88, $22, $88, $aa, $aa, $01, $01, $51, $45, $45
               .byte $41, $40, $40, $c2, $04, $06, $07, $1e, $1f, $1e, $c2, $07, $fd, $f4, $01, $c2
               .byte $03, $04, $06, $04, $06, $06, $00, $00, $22, $88, $22, $8a, $a9, $66, $01, $89
               .byte $21, $8d, $31, $ed, $bd, $f4, $c2, $06, $01, $06, $07, $00, $00, $88, $22, $88
               .byte $aa, $66, $99, $00, $00, $80, $22, $aa, $aa, $66, $99, $10, $10, $12, $10, $92
               .byte $62, $62, $61, $3f, $3e, $1f, $c2, $03, $1e, $0f, $0e, $ba, $fe, $b9, $fd, $b9
               .byte $bd, $b9, $ad, $f3, $c3, $c3, $cc, $0c, $c2, $03, $0f, $00, $00, $08, $22, $88
               .byte $aa, $ee, $bb, $00, $00, $44, $11, $45, $55, $99, $65, $c2, $03, $80, $c2, $0d
               .byte $00, $c2, $04, $10, $50, $40, $41, $41, $40, $40, $62, $48, $62, $6a, $bb, $ee
               .byte $00, $00, $22, $88, $2a, $aa, $99, $66, $02, $02, $32, $c6, $f5, $f6, $f5, $f5
               .byte $06, $06, $04, $24, $24, $26, $14, $12, $40, $c2, $04, $00, $11, $44, $11, $c2
               .byte $03, $30, $18, $62, $48, $6a, $6a, $00, $00, $22, $88, $22, $8a, $aa, $99, $01
               .byte $01, $31, $c4, $37, $f4, $f7, $e7, $00, $08, $20, $08, $2a, $8a, $29, $a9, $c2
               .byte $06, $6f, $bf, $af, $c2, $03, $f4, $e4, $e4, $c2, $03, $d0, $3f, $3d, $b7, $b5
               .byte $9d, $b7, $95, $9d, $aa, $99, $66, $99, $55, $56, $65, $99, $aa, $99, $66, $55
               .byte $99, $56, $55, $65, $f0, $9c, $67, $59, $65, $59, $65, $55, $56, $95, $55, $5d
               .byte $75, $7d, $f5, $7f, $55, $65, $55, $57, $d5, $77, $f5, $7f, $96, $c2, $03, $55
               .byte $dd, $57, $dd, $77, $a1, $a3, $a1, $a3, $c2, $03, $a1, $a0, $02, $08, $02, $22
               .byte $88, $22, $88, $62, $21, $89, $22, $88, $a2, $8a, $aa, $a9, $bb, $ae, $7b, $6e
               .byte $9b, $9e, $67, $99, $90, $a5, $aa, $bb, $ae, $bb, $ae, $aa, $5f, $fe, $fb, $fe
               .byte $fb, $ee, $fb, $ed, $f4, $f6, $f4, $d2, $d8, $d2, $68, $6a, $1b, $1e, $9b, $1e
               .byte $9b, $9e, $9b, $7f, $99, $66, $99, $67, $5d, $57, $5d, $77, $f4, $f6, $f4, $f6
               .byte $f4, $f6, $da, $da, $86, $27, $86, $27, $87, $a7, $9f, $9f, $66, $95, $55, $5d
               .byte $77, $5d, $77, $dd, $ab, $ae, $bb, $ef, $bf, $ff, $bd, $ed, $62, $61, $c2, $04
               .byte $41, $81, $81, $07, $27, $87, $27, $ab, $25, $a9, $a5, $fd, $fd, $f5, $f4, $d4
               .byte $f4, $d4, $d4, $05, $15, $05, $47, $15, $57, $1d, $57, $66, $99, $66, $95, $57
               .byte $55, $77, $5d, $a9, $a9, $ba, $ae, $bb, $c2, $03, $ff, $00, $55, $aa, $ee, $bb
               .byte $ff, $ff, $fb, $00, $55, $aa, $aa, $ba, $ee, $fa, $aa, $61, $49, $61, $49, $61
               .byte $69, $67, $67, $ee, $ba, $ea, $a9, $a6, $a9, $a6, $99, $95, $57, $5d, $77, $df
               .byte $7f, $df, $77, $e4, $a4, $e4, $d4, $d0, $d0, $50, $40, $18, $1a, $16, $99, $96
               .byte $99, $56, $55, $89, $a9, $65, $99, $65, $99, $55, $55, $fb, $ee, $bb, $ee, $ba
               .byte $ea, $a9, $a6, $66, $99, $55, $55, $57, $dd, $77, $dd, $c2, $04, $a5, $bb, $ee
               .byte $bf, $ee, $75, $dd, $75, $55, $c2, $04, $aa, $6a, $59, $6a, $a9, $aa, $a9, $a5
               .byte $99, $90, $90, $93, $93, $c2, $04, $43, $c2, $03, $55, $75, $d7, $77, $df, $77
               .byte $75, $57, $55, $95, $59, $96, $5a, $a6, $5d, $55, $55, $65, $59, $65, $59, $a6
               .byte $75, $c2, $03, $55, $95, $66, $95, $66, $c2, $03, $55, $00, $32, $32, $30, $f2
               .byte $c2, $03, $55, $00, $8c, $0c, $8c, $8f, $99, $a6, $99, $aa, $aa, $00, $2a, $2a
               .byte $90, $60, $a8, $98, $a8, $8a, $aa, $aa, $4a, $62, $1a, $19, $06, $01, $c0, $f0
               .byte $a6, $a9, $6a, $99, $66, $99, $55, $05, $66, $99, $65, $97, $5d, $57, $5d, $75
               .byte $5f, $55, $99, $66, $9a, $6a, $aa, $55, $56, $6a, $ab, $ad, $b9, $a5, $51, $01
               .byte $6a, $69, $66, $a9, $a6, $99, $a6, $55, $7f, $55, $5a, $66, $5a, $66, $5a, $55
               .byte $ff, $55, $a9, $a9, $a5, $a5, $95, $55, $da, $5a, $69, $66, $a9, $a6, $a9, $55
               .byte $9f, $95, $99, $a5, $99, $65, $99, $55, $ff, $55, $6a, $9a, $6a, $9a, $69, $55
               .byte $fd, $55, $56, $54, $58, $60, $80, $00, $c2, $03, $01, $c2, $03, $02, $00, $00
               .byte $a9, $a6, $69, $66, $69, $6a, $6a, $55, $54, $56, $5a, $5a, $6b, $6a, $aa, $df
               .byte $95, $95, $99, $66, $99, $66, $a9, $c2, $03, $55, $66, $59, $56, $55, $56, $c2
               .byte $03, $55, $66, $99, $66, $9a, $aa, $c2, $03, $55, $66, $99, $6a, $aa, $aa, $55
               .byte $57, $57, $67, $a7, $97, $67, $97, $57, $a7, $a5, $a6, $a9, $a6, $99, $a6, $55
               .byte $ff, $55, $5a, $66, $5a, $66, $5a, $55, $ff, $55, $a5, $a4, $a4, $90, $40, $00
               .byte $42, $42, $02, $c2, $05, $01, $56, $59, $56, $5a, $66, $9a, $66, $55, $65, $99
               .byte $66, $99, $aa, $66, $99, $55, $ff, $55, $55, $56, $59, $66, $5a, $55, $ff, $55
               .byte $99, $66, $99, $66, $aa, $55, $ff, $55, $99, $66, $9a, $aa, $aa, $55, $ff, $55
               .byte $99, $66, $c2, $03, $aa, $55, $ff, $55, $c2, $03, $a4, $90, $90, $40, $4f, $4f
               .byte $0f, $0f, $3f, $3f, $ff, $ff, $aa, $99, $c2, $03, $aa, $88, $aa, $aa, $99, $a6
               .byte $9a, $aa, $aa, $80, $aa, $aa, $a9, $9a, $a9, $aa, $aa, $00, $aa, $aa, $99, $66
               .byte $9a, $a9, $6a, $00, $aa, $aa, $c2, $06, $e9, $c2, $04, $59, $c2, $03, $e3, $e9
               .byte $9e, $9e, $63, $63, $30, $30, $c2, $04, $00, $e9, $e9, $c2, $04, $9e, $c2, $0f
               .byte $e9, $c2, $05, $59, $c2, $03, $e3, $e9, $9e, $9e, $1e, $3e, $6e, $6e, $60, $00
               .byte $60, $3e, $e9, $e9, $c2, $04, $3e, $c2, $05, $e3, $9e, $c2, $04, $e9, $c2, $05
               .byte $e3, $d9, $c2, $0b, $59, $39, $59, $59, $c2, $03, $7a, $3e, $e6, $c2, $05, $59
               .byte $e9, $c2, $05, $59, $e9, $e0, $c2, $07, $e3, $e9, $c2, $0e, $59, $c2, $04, $6a
               .byte $36, $59, $59, $5d, $c2, $03, $59, $5d, $5d, $c2, $03, $59, $e9, $c2, $03, $e3
               .byte $9e, $9e, $de, $ed, $e3, $e3, $c2, $05, $e9, $d9, $c2, $03, $e9, $d9, $e9, $59
               .byte $59, $e9, $39, $d9, $39, $6a, $e6, $e9, $39, $d9, $e9, $e3, $59, $39, $c2, $04
               .byte $e9, $d5, $e3, $e3, $93, $c2, $03, $5d, $35, $60, $ed, $a6, $ea, $e7, $e6, $e6
               .byte $a6, $e6, $e6, $a6, $a6, $e6, $6d, $ed, $eb, $b9, $95, $e9, $00, $e6, $e6, $a6
               .byte $e3, $e6, $60, $e6, $a6, $ea, $e6, $ea, $ea, $59, $c2, $06, $5d, $e5, $c2, $03
               .byte $e6, $ea, $e7, $e6, $e6, $a6, $e6, $e6, $a6, $ea, $c2, $03, $e6, $9b, $59, $c2
               .byte $03, $95, $e6, $7a, $a6, $ea, $e6, $e6, $7a, $a6, $ea, $e7, $a7, $e7, $5d, $c2
               .byte $03, $59, $95, $95, $59, $59, $c2, $03, $e6, $e3, $e3, $e6, $e3, $e3, $e6, $e6
               .byte $e3, $c2, $04, $e6, $96, $c2, $04, $98, $e6, $e3, $e3, $c2, $03, $e6, $c2, $05
               .byte $e3, $e5, $c2, $04, $59, $c2, $06, $01, $0e, $00, $0d, $0e, $c2, $03, $09, $03
               .byte $03, $c2, $09, $01, $c2, $06, $03, $c2, $0f, $01, $0e, $0d, $00, $0d, $0d, $c2
               .byte $03, $01, $c2, $03, $03, $06, $01, $03, $c2, $04, $01, $06, $03, $03, $09, $00
               .byte $00, $09, $01, $00, $c2, $03, $01, $03, $03, $c2, $05, $01, $c2, $03, $09, $0e
               .byte $0d, $00, $c2, $09, $0d, $01, $00, $0d, $c2, $03, $0e, $01, $01, $0e, $c2, $04
               .byte $0d, $01, $c2, $03, $0d, $00, $00, $03, $01, $c2, $03, $09, $c2, $04, $01, $03
               .byte $0d, $c2, $04, $00, $0d, $01, $00, $00, $0d, $0d, $00, $00, $0d, $0e, $07, $0e
               .byte $0e, $01, $00, $01, $09, $c2, $03, $00, $09, $09, $c2, $03, $0d, $03, $c2, $03
               .byte $01, $c2, $04, $03, $00, $0d, $03, $c2, $03, $05, $c2, $03, $03, $05, $03, $03
               .byte $05, $0d, $0d, $05, $c2, $03, $06, $0e, $01, $c2, $03, $05, $01, $01, $0d, $05
               .byte $05, $01, $05, $0d, $03, $0d, $09, $00, $09, $01, $01, $0d, $01, $03, $01, $01
               .byte $03, $07, $0a, $01, $0a, $0a, $01, $01, $0a, $05, $05, $09, $05, $0b, $c2, $03
               .byte $01, $0a, $01, $06, $01, $01, $03, $01, $06, $01, $07, $07, $0d, $01, $01, $c2
               .byte $04, $09, $03, $01, $01, $0a, $03, $03, $07, $0a, $07, $07, $0a, $07, $07, $01
               .byte $03, $0d, $05, $c2, $04, $0d, $0a, $06, $07, $07, $03, $03, $06, $07, $07, $06
               .byte $01, $09, $09, $c2, $05, $0d, $01, $01, $09, $01, $03, $0a, $07, $01, $c2, $06
               .byte $0a, $01, $03, $09, $c2, $04, $0a, $0e, $c2, $03, $0a, $00, $01, $01, $c2, $05
               .byte $0a, $09, $01, $c2, $00

                        // character set data (64 characters + 15 for stars) -----------------]
                        // -------------------------------------------------------------------]
char_set_data:          // font data - arena a 
                        //
                        // removed from source release
                        // includes characters are star data & lines
               .byte $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00, $00, $00, $00, $00, $00
               .byte $00, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $00, $00, $00, $00, $00
               .byte $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $00, $00, $00, $00
               .byte $00, $00, $00, $00, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $ff, $00
               .byte $00, $00, $00, $00, $ff, $00, $00, $00

                        // scroller text message ---------------------------------------------]
                        // -------------------------------------------------------------------]
message:       .byte 129, 129, 129, 129, 129, 142, 132, 160, 136, 133, 146, 133, 160, 151, 133, 160
               .byte 129, 146, 133, 174, 174, 174, 174, 160, 129, 142, 143, 148, 136, 133, 146, 160
               .byte 147, 153, 142, 148, 129, 152, 160, 144, 129, 146, 148, 153, 160, 137, 147, 160
               .byte 149, 144, 143, 142, 160, 149, 147, 160, 151, 137, 148, 136, 160, 147, 143, 141
               .byte 133, 160, 142, 133, 151, 160, 144, 146, 143, 132, 149, 131, 148, 137, 143, 142
               .byte 147, 160, 146, 133, 140, 133, 129, 147, 133, 132, 160, 148, 143, 160, 147, 148
               .byte 137, 141, 149, 140, 129, 148, 133, 160, 129, 142, 132, 160, 132, 133, 140, 137
               .byte 135, 136, 148, 160, 153, 143, 149, 146, 160, 147, 133, 142, 147, 133, 147, 174
               .byte 174, 174, 174, 160, 134, 137, 146, 147, 148, 140, 153, 172, 160, 148, 136, 146
               .byte 133, 133, 160, 131, 136, 133, 133, 146, 147, 160, 148, 143, 160, 150, 133, 131
               .byte 148, 146, 133, 152, 160, 134, 143, 146, 160, 148, 136, 137, 147, 160, 153, 133
               .byte 129, 146, 167, 147, 160, 129, 151, 133, 147, 143, 141, 133, 160, 150, 133, 142
               .byte 149, 133, 161, 161, 160, 142, 143, 151, 172, 160, 140, 133, 148, 160, 141, 133
               .byte 160, 144, 146, 133, 147, 133, 142, 148, 160, 148, 143, 160, 153, 143, 149, 172
               .byte 160, 129, 160, 146, 129, 148, 136, 133, 146, 160, 184, 176, 167, 147, 160, 137
               .byte 142, 147, 144, 137, 146, 133, 132, 160, 137, 142, 148, 146, 143, 160, 129, 142
               .byte 132, 160, 143, 134, 134, 137, 131, 129, 140, 140, 153, 160, 141, 153, 160, 134
               .byte 137, 146, 147, 148, 160, 131, 182, 180, 160, 146, 133, 140, 133, 129, 147, 133
               .byte 160, 160, 170, 170, 160, 147, 153, 142, 148, 146, 143, 160, 170, 170, 160, 160
               .byte 144, 146, 143, 149, 132, 140, 153, 160, 134, 137, 140, 140, 137, 142, 135, 160
               .byte 153, 143, 149, 146, 160, 147, 131, 146, 133, 133, 142, 147, 160, 129, 148, 160
               .byte 147, 153, 142, 148, 129, 152, 163, 177, 180, 160, 173, 160, 130, 146, 143, 149
               .byte 135, 136, 148, 160, 148, 143, 160, 153, 143, 149, 160, 130, 153, 160, 138, 133
               .byte 147, 132, 133, 146, 160, 173, 160, 129, 134, 148, 133, 146, 160, 129, 148, 148
               .byte 133, 142, 132, 137, 142, 135, 160, 147, 153, 142, 148, 129, 152, 163, 177, 179
               .byte 172, 160, 137, 160, 151, 129, 147, 160, 137, 142, 147, 144, 137, 146, 133, 132
               .byte 160, 148, 143, 160, 151, 146, 137, 148, 133, 160, 141, 153, 160, 134, 137, 146
               .byte 147, 148, 160, 131, 182, 180, 160, 132, 133, 141, 143, 174, 160, 134, 143, 149
               .byte 146, 160, 151, 133, 133, 139, 147, 160, 129, 135, 143, 160, 137, 160, 146, 133
               .byte 129, 140, 137, 147, 133, 132, 160, 137, 160, 136, 129, 132, 160, 147, 144, 133
               .byte 142, 148, 160, 177, 177, 160, 141, 143, 142, 148, 136, 147, 160, 132, 143, 137
               .byte 142, 135, 160, 129, 142, 153, 148, 136, 137, 142, 135, 160, 130, 149, 148, 160
               .byte 151, 143, 146, 139, 137, 142, 135, 160, 143, 142, 160, 137, 148, 174, 174, 174
               .byte 160, 147, 143, 160, 137, 160, 134, 143, 131, 149, 147, 133, 132, 160, 129, 142
               .byte 132, 160, 132, 137, 150, 133, 132, 160, 130, 129, 131, 139, 160, 137, 142, 148
               .byte 143, 160, 148, 136, 133, 160, 131, 182, 180, 160, 136, 129, 146, 132, 151, 129
               .byte 146, 133, 174, 174, 174, 160, 129, 134, 148, 133, 146, 160, 147, 143, 141, 133
               .byte 160, 147, 131, 146, 129, 141, 130, 140, 137, 142, 135, 172, 160, 136, 133, 146
               .byte 133, 160, 137, 160, 129, 141, 160, 148, 143, 142, 137, 135, 136, 148, 160, 151
               .byte 137, 148, 136, 160, 129, 142, 160, 149, 142, 137, 142, 147, 144, 137, 146, 137
               .byte 142, 135, 172, 160, 147, 153, 142, 148, 129, 152, 160, 137, 142, 147, 144, 137
               .byte 146, 133, 132, 160, 143, 140, 132, 160, 147, 139, 143, 143, 140, 160, 137, 142
               .byte 148, 146, 143, 174, 174, 174, 174, 174, 160, 160, 160, 160, 160, 160, 133, 142
               .byte 138, 143, 153, 174, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160
               .byte 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160
               .byte 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 255

                        // starfield generation data - temp data - can be blown away post init -]
                        // ---------------------------------------------------------------------]
star_colour_ram_copy:   .byte 001, 000, 000, 000, 000, 012, 012, 015
                        .byte 001, 000, 000, 000, 000, 015, 015, 015
                        .byte 003, 000, 000, 000, 000, 003, 003, 003
                        .byte 014, 000, 000, 000, 000, 014, 014, 014
                        .byte 004, 000, 000, 000, 000, 004, 004, 004
                        .byte 006, 000, 000, 000, 000, 006, 006, 006

starmap_patrn1:         .byte 064, 065, 066, 067, 068, 064, 065, 066
                        .byte 067, 068, 064, 065, 066, 067, 068, 064
                        .byte 065, 066, 067, 068, 064, 065, 066, 067
                        .byte 068, 064, 065, 066, 067, 068, 064, 065
                        .byte 066, 067, 068, 064, 065, 066, 067, 068
                        .byte 064, 065, 066
starmap_patrn2:         .byte 069, 070, 071, 072, 073, 069, 070, 071
                        .byte 072, 073, 069, 070, 071, 072, 073, 069
                        .byte 070, 071, 072, 073, 069, 070, 071, 072
                        .byte 073, 069, 070, 071, 072, 073, 069, 070
                        .byte 071, 072, 073, 069, 070, 071, 072, 073
                        .byte 069, 070, 071
starmap_patrn3:         .byte 074, 075, 076, 077, 078, 074, 075, 076
                        .byte 077, 078, 074, 075, 076, 077, 078, 074
                        .byte 075, 076, 077, 078, 074, 075, 076, 077
                        .byte 078, 074, 075, 076, 077, 078, 074, 075
                        .byte 076, 077, 078, 074, 075, 076, 077, 078
                        .byte 074, 075, 076

colour_patrn:           .byte 011, 011, 012, 012, 015, 015, 001, 015, 015, 012
                        .byte 015, 012, 011, 011, 011, 011, 011, 011, 012, 011
                        .byte 011, 011, 011, 011, 012, 011, 011, 011, 011, 012
                        .byte 015, 001, 001, 015, 012, 012, 012, 012, 012, 012
                        .byte 012, 012, 012, 012, 012, 011, 012, 012, 015, 012
                        .byte 012, 012, 012, 015, 001, 001, 001, 015, 015, 015
                        .byte 015, 012, 012, 011, 011, 011, 011, 011, 012, 012
                        .byte 015, 015, 015, 015, 015, 015, 001, 015, 015, 012
                        .byte 011, 011, 011, 012, 012, 012, 012, 012, 011, 011

                        // demo cycle messages --------------------------------------------]
                        // ----------------------------------------------------------------]
textcycle_messages_1: .text "   code: jesder   "
                      .text "  no multi parts  "
                      .text "just a small intro"
                      .text "  no rasters were "
                      .text " full source code "
                      .text " greets out to... "
                      .text "booze design, mon "
                      .text "the dreams, triad "
                      .text "my first c64 demo "
                      .text "let's read it all "

textcycle_messages_2: .text "    logo: jsl     "
                      .text "    no 3d polys   "
                      .text "    * syntro *    "
                      .text "harmed when making"
                      .text "   available as   "
                      .text "biorhythm, camelot"
                      .text "  censor, cosine  "
                      .text "crest, razor 1911 "
                      .text " but not my last  "
                      .text " again, or reset  "

textcycle_messages_3: .text " music: freekzoid "
                      .text "  no multiplexer  "
                      .text "      enjoy.      "
                      .text "    this intro    "
                      .text "  www.0xc64.com   "
                      .text "defame, onslaught "
                      .text "fairlight, oxyron "
                      .text " the syntax crew  "
                      .text "see you next year "
                      .text "  your choice :)  "

                        // support tables ----------------------------------------------]
                        // -------------------------------------------------------------]
costable:               .byte 063, 063, 063, 063, 063, 063, 062, 062
                        .byte 061, 060, 060, 059, 058, 057, 056, 055
                        .byte 054, 053, 052, 051, 049, 048, 047, 045
                        .byte 044, 042, 041, 039, 038, 036, 035, 033
                        .byte 032, 030, 028, 027, 025, 024, 022, 021
                        .byte 019, 018, 016, 015, 014, 012, 011, 010
                        .byte 009, 008, 007, 006, 005, 004, 003, 003
                        .byte 002, 001, 001, 000, 000, 000, 000, 000
                        .byte 000, 000, 000, 000, 000, 000, 001, 001
                        .byte 002, 003, 003, 004, 005, 006, 007, 008
                        .byte 009, 010, 011, 012, 014, 015, 016, 018
                        .byte 019, 021, 022, 024, 025, 027, 028, 030
                        .byte 031, 033, 035, 036, 038, 039, 041, 042
                        .byte 044, 045, 047, 048, 049, 051, 052, 053
                        .byte 054, 055, 056, 057, 058, 059, 060, 060
                        .byte 061, 062, 062, 063, 063, 063, 063, 063

seq_cell_screen_ptr:    .byte $00, $04, $04, $04, $08, $04, $0c, $04
                        .byte $10, $04, $14, $04, $18, $04, $1c, $04
                        .byte $20, $04, $24, $04, $a0, $04, $a4, $04
                        .byte $a8, $04, $ac, $04, $b0, $04, $b4, $04
                        .byte $b8, $04, $bc, $04, $c0, $04, $c4, $04
                        .byte $40, $05, $44, $05, $48, $05, $4c, $05
                        .byte $50, $05, $54, $05, $58, $05, $5c, $05
                        .byte $60, $05, $64, $05, $e0, $05, $e4, $05
                        .byte $e8, $05, $ec, $05, $f0, $05, $f4, $05
                        .byte $f8, $05, $fc, $05, $00, $06, $04, $06
                        .byte $80, $06, $84, $06, $88, $06, $8c, $06
                        .byte $90, $06, $94, $06, $98, $06, $9c, $06
                        .byte $a0, $06, $a4, $06, $20, $07, $24, $07
                        .byte $28, $07, $2c, $07, $30, $07, $34, $07
                        .byte $38, $07, $3c, $07, $40, $07, $44, $07


                        // music data - 3165 bytes ----------------------------------------]
                        // ----------------------------------------------------------------]
music_data:             // CityBomber - Paul Hannay (Freekzoid) - released 1992
                        // pulled from High Voltage SID collection (www.hvsc.c64.org)
                        .import binary "ode to 64.bin"
                        // removed from source release