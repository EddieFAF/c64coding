// Erste Versuche mit Zeichensatz
BasicUpstart2(start)

.var music = LoadSid("R_I_S_K.sid")    //<- Here we load the sid file

start:
    jsr $e544 // Bildschirm löschen

    lda #00
    sta $d020
    sta $d021
    rts         // back to BASIC


      *=music.location "Music"
music_data:      .fill music.size, music.getData(i)

// Print the music info while assembling
.print ""
.print "SID Data"
.print "--------"
.print "location=$"+toHexString(music.location)
.print "init=$"+toHexString(music.init)
.print "play=$"+toHexString(music.play)
.print "songs="+music.songs
.print "startSong="+music.startSong
.print "size=$"+toHexString(music.size)
.print "length=$"+toHexString(music.location)+"-$"+toHexString(music.size+music.location)