;
; Two-player Tetris Mod
;

; Normally player 1 uses palette 2 and 6 (zero indexed). The mod uses palettes
; 1/2 and 5/6, but has player 1 uses palette 1 so that we can just use the
; "current player" as the palette value.

; TODO:
; Save another RNG to let the behind player catch up.
; Handle end-game. If one player dies, if the player behind in score is still playing, they can keep playing. Unclear if score should be the only way. People may care about lines, or some other such. Need to think about it more. If let both players go to end, then may want to let 2nd player enter high score
; Fix background tetrimino pattern
; Figure out what's up with twoPlayerPieceDelayCounter
; Demo can be two-player if second player presses start and then the system goes idle. But demo playing is broken in 2 player
; Allow toggling on garbage?
; Allow second player to disable next piece display (minor)

; Integrations:
; Have handicap support 2 players
; Any way to fit stats on screen? Seems like there's no room.
; No room for A/B-Type, high score

.include "build/tetris.inc"

.bss


.segment "GAMEBSS"

.res 1 ; must be at least size 1 to prevent init loop from breaking

.code

initGameState_mod:
        .export initGameState_mod
        .import __GAMEBSS_SIZE__, __GAMEBSS_RUN__
        jsr     memset_page

        lda     #$00
        ldx     #<__GAMEBSS_SIZE__
@clearByte:
        sta     __GAMEBSS_RUN__-1,x
        dex
        bne     @clearByte

        .importzp personal_rng
        .importzp spawnID_
        .importzp spawnCount_
        .importzp player1_rng
        .importzp player1_spawnID_
        .importzp player1_spawnCount_
        .importzp player2_rng
        .importzp player2_spawnID_
        .importzp player2_spawnCount_
; FIXME. reuses the seed at beginning of game
        lda     rng_seed
        sta     personal_rng
        sta     player1_rng
        sta     player2_rng
        lda     rng_seed+1
        sta     personal_rng+1
        sta     player1_rng+1
        sta     player2_rng+1
        lda     spawnID_
        sta     player1_spawnID_
        sta     player2_spawnID_
        lda     spawnCount_
        sta     player1_spawnCount_
        sta     player2_spawnCount_

        ldx     #player1_rng
        ldy     #$02
        jsr     generateNextPseudorandomNumber
        ldx     #player1_rng
        ldy     #$02
        jsr     generateNextPseudorandomNumber
        ldx     #player2_rng
        ldy     #$02
        jsr     generateNextPseudorandomNumber
        ldx     #player2_rng
        ldy     #$02
        jsr     generateNextPseudorandomNumber

        rts

initGameBackground_mod:
        .export initGameBackground_mod
        lda     numberOfPlayers
        cmp     #$01
        bne     @twoPlayers
        jsr     bulkCopyToPpu
        .addr   game_nametable
        .import after_initGameBackground_mod_player1
        jmp     after_initGameBackground_mod_player1

@twoPlayers:
        jsr     copyNametableToPpu
        .addr   twoplayer_game_nametable
        .import after_initGameBackground_mod_player2
        jmp     after_initGameBackground_mod_player2

twoplayer_game_nametable:
        .incbin "twoplayer_game.nam"

copyNametableToPpu:
        jsr     copyAddrAtReturnAddressToTmp_incrReturnAddrBy2
        ldx     PPUSTATUS
        lda     #$20
        sta     PPUADDR
        lda     #$00
        sta     PPUADDR
        jsr     copyPageToPpu
        inc     tmp2
        jsr     copyPageToPpu
        inc     tmp2
        jsr     copyPageToPpu
        inc     tmp2
        jsr     copyPageToPpu
        rts

copyPageToPpu:
        ldy     #$00
@copyByte:
        lda     (tmp1),y
        sta     PPUDATA
        iny
        bne     @copyByte
        rts

renderPlay_mod:
        .export renderPlay_mod
        lda     numberOfPlayers
        cmp     #$02
        beq     @twoPlayers
        .import after_renderPlay_mod
        jmp     after_renderPlay_mod

@twoPlayers:
        ; Update level/palette on a different frame than score (and implicitly
        ; lines) is updated. This reduces the number of updates on the same
        ; frame to help squeeze updates within vsync. The wrong palette is not
        ; visible the first frame of the game in two-player because the game
        ; logic runs after rendering, so the first frame has no sprites. In
        ; one-player the statistics will have the wrong palette for an extra
        ; frame.
        lda     outOfDateRenderFlags
        eor     #$02
        and     #$06
        bne     @renderScore
        lda     #$20
        sta     PPUADDR
        lda     #$EF
        sta     PPUADDR
        ldx     player1_levelNumber
        lda     levelDisplayTable,x
        jsr     twoDigsToPPU
        jsr     updatePaletteForLevel
        lda     #$22
        sta     PPUADDR
        lda     #$50
        sta     PPUADDR
        ldx     player2_levelNumber
        lda     levelDisplayTable,x
        jsr     twoDigsToPPU
        jsr     updatePaletteForLevel_player2
        lda     outOfDateRenderFlags
        and     #$FD
        sta     outOfDateRenderFlags

@renderScore:
        lda     outOfDateRenderFlags
        and     #$04
        beq     @ret

        lda     #$20
        sta     PPUADDR
        lda     #$66
        sta     PPUADDR
        lda     player1_score+2
        jsr     twoDigsToPPU
        lda     player1_score+1
        jsr     twoDigsToPPU
        lda     player1_score
        jsr     twoDigsToPPU

        lda     #$20
        sta     PPUADDR
        lda     #$78
        sta     PPUADDR
        lda     player2_score+2
        jsr     twoDigsToPPU
        lda     player2_score+1
        jsr     twoDigsToPPU
        lda     player2_score
        jsr     twoDigsToPPU

        lda     outOfDateRenderFlags
        and     #$FB
        sta     outOfDateRenderFlags

@ret:
        lda     #$00
        jmp     after_renderPlay_mod

updatePaletteForLevel_player2:
        lda     player2_levelNumber
        ldy     #$08
        .import updatePaletteForLevel_mod10
        jmp     updatePaletteForLevel_mod10

.import vramPlayfieldRowsHi
.import vramPlayfieldRowsLo
.define FASTROWTOVRAM 2
.if FASTROWTOVRAM = 1
; A faster implementation of copying to the VRAM, by 92 cycles (from 246
; cycles down to 154).
;
; An unmodified implementation of render_mode_play_and_demo, but with 2 players
; enabled, does not fit within a vsync because 8 rows are copied which totals
; 1968 cycles by itself. The optimization here brings it down to 1232 cycles
; which may still be tight but isn't too much more than the 984 cycles
; normally used for 1 player.
copyPlayfieldRowToVRAM_fast:
        .export copyPlayfieldRowToVRAM_fast
        ldx     vramRow
        cpx     #$15
        bmi     @skipRts
        rts
@skipRts:
        ldy     multBy10Table,x
        lda     vramPlayfieldRowsHi,x
        sta     PPUADDR
        lda     numberOfPlayers
        cmp     #$01
        beq     @onePlayer
        lda     playfieldAddr+1
        cmp     #$05
        beq     @playerTwo
        lda     vramPlayfieldRowsLo,x
        sec
        sbc     #$04
        sta     PPUADDR
        jmp     @copyRowForPlayer1

@playerTwo:
        lda     vramPlayfieldRowsLo,x
        clc
        adc     #$0E
        sta     PPUADDR
        .repeat 10,I
        lda     playfieldForSecondPlayer+I,y
        sta     PPUDATA
        .endrepeat
        jmp     @doneWithRow

@onePlayer:
        lda     vramPlayfieldRowsLo,x
        clc
        adc     #$06
        sta     PPUADDR
@copyRowForPlayer1:
        .repeat 10,I
        lda     playfield+I,y
        sta     PPUDATA
        .endrepeat
@doneWithRow:
        inc     vramRow
        lda     vramRow
        cmp     #$14
        bmi     @ret
        lda     #$20
        sta     vramRow
@ret:   rts

.elseif FASTROWTOVRAM = 2
; A batched implementation of copying playfield to the VRAM, saving 76 cycles
; compared to copying each row individually (from 635 cycles down to 559,
; including boilerplate in caller).
;
; reg x: (input/output) vramRow
; reg a: (input) 0=player 1, 1=player 2
;
copyPlayfieldRowToVRAM4:
        .export copyPlayfieldRowToVRAM4
        cpx     #$20
        bmi     @skipRts
        rts
@skipRts:
        sta     generalCounter3
        tay
        beq     @playerOne
;playerTwo:
        lda     #$0E
        sta     generalCounter2
        bne     @continueSetup

@playerOne:
        ldy     numberOfPlayers
        lda     @offsetTable-1,y
        sta     generalCounter2

@continueSetup:
        lda     #$04
        sta     generalCounter
; reg x: vramRow
; reg y: playfield offset for row
; generalCounter: loop counter
; generalCounter2: VRAM LO offset
; generalCounter3: 0=player 1, 1=player 2
@loop:
        lda     vramPlayfieldRowsHi,x
        sta     PPUADDR
        lda     vramPlayfieldRowsLo,x
        clc
        adc     generalCounter2
        sta     PPUADDR

        ldy     multBy10Table,x
        lda     generalCounter3
        bne     @playerTwoCopy
        .repeat 10,I
        lda     playfield+I,y
        sta     PPUDATA
        .endrepeat
        jmp     @nextIter
@playerTwoCopy:
        .repeat 10,I
        lda     playfieldForSecondPlayer+I,y
        sta     PPUDATA
        .endrepeat

@nextIter:
        inx
        cpx     #$14
        bpl     @doneWithAllRows
@vramInRange:
        dec     generalCounter
        beq     @ret
        jmp     @loop

@doneWithAllRows:
        ldx     #$20
@ret:
        rts

@offsetTable:
        .byte   $06,(-$04)&$FF
.endif


renderTetrisFlashAndSound_mod:
        .export renderTetrisFlashAndSound_mod
        lda     player1_completedLines
        cmp     #$04
        beq     @ret
        lda     player2_completedLines
        cmp     #$04
@ret:
        .import after_renderTetrisFlashAndSound_mod
        jmp     after_renderTetrisFlashAndSound_mod

.segment "CODE2"

stageSpriteForNextPiece_player1_mod:
        .export stageSpriteForNextPiece_player1_mod
        lda     displayNextPiece
        bne     @ret
        lda     numberOfPlayers
        cmp     #$01
        bne     @twoPlayers
        lda     #$C8
        sta     spriteXOffset
        lda     #$77
        sta     spriteYOffset
        jmp     @stage
@twoPlayers:
        lda     #$78
        sta     spriteXOffset
        lda     #$53
        sta     spriteYOffset
@stage:
        .importzp player1_nextPiece
        ldx     player1_nextPiece
        lda     orientationToSpriteTable,x
        sta     spriteIndexInOamContentLookup

        ; Alternate draw order to flicker on conflict
        lda     frameCounter
        and     #$01
        beq     @currentGoesLast
        jmp     loadSpriteIntoOamStaging

@currentGoesLast:
        ; Place sprite at end of oamStaging, to make it fail to render when
        ; sprites align, instead of player2's current piece
        lda     oamStagingLength
        sta     tmp1
        lda     #$100-4*4
        sta     oamStagingLength
        jsr     loadSpriteIntoOamStaging
        lda     tmp1
        sta     oamStagingLength

@ret:   rts

savePlayer2State_mod:
        .export savePlayer2State_mod
        jsr     savePlayer2State
        jsr     stageSpriteForNextPiece_player2
        rts

stageSpriteForNextPiece_player2:
        lda     displayNextPiece
        bne     @ret
        lda     #$80
        sta     spriteXOffset
        lda     #$AB
        sta     spriteYOffset
        .importzp player2_nextPiece
        ldx     player2_nextPiece
        lda     orientationToSpriteTable,x
        sta     spriteIndexInOamContentLookup
        jmp     loadSpriteIntoOamStaging_player2

@ret:   rts


loadSpriteIntoOamStaging_player2:
        lda     oamStagingLength
        sta     generalCounter3
        jsr     loadSpriteIntoOamStaging
        ldx     generalCounter3
@adjustSprite:
        inx
        inx
        inc     oamStaging,x
        inx
        inx
        cpx     oamStagingLength
        bne     @adjustSprite
        rts


pickRandomTetrimino_mod:
        .export pickRandomTetrimino_mod
        ldx     #rng_seed
        ldy     #$02
        jsr     generateNextPseudorandomNumber
        ldx     #personal_rng
        ldy     #$02
        jsr     generateNextPseudorandomNumber
        rts

gameMode_levelMenu_nametable_mod:
        .export gameMode_levelMenu_nametable_mod
        jsr     bulkCopyToPpu
        .addr   level_menu_nametable
        lda     numberOfPlayers
        cmp     #$01
        bne     @twoPlayers

        jsr     bulkCopyToPpu
        .addr   player2PressStartPatch
        jmp     @levelMenuInit

@twoPlayers:
        jsr     bulkCopyToPpu
        .addr   player1ActivePatch

@levelMenuInit:
        lda     player2_startLevel
@forceStartLevelToRange:
        sta     player2_startLevel
        sec
        sbc     #$0A
        bcs     @forceStartLevelToRange

        rts

gameMode_levelMenu_processPlayer1Navigation_processPlayer2:
        .export gameMode_levelMenu_processPlayer1Navigation_processPlayer2
        lda     newlyPressedButtons_player2
        cmp     #$10
        bne     @checkBPressed
        lda     numberOfPlayers
        cmp     #$01
        bne     @checkBPressed
        inc     numberOfPlayers
        lda     #$08
        sta     soundEffectSlot1Init
        jmp     gameMode_levelMenu
@checkBPressed:
        lda     newlyPressedButtons_player2
        cmp     #$40
        bne     @ret
        lda     numberOfPlayers
        cmp     #$02
        bne     @ret
        dec     numberOfPlayers
        lda     #$01
        sta     soundEffectSlot3Init
        jmp     gameMode_levelMenu
@ret:
        jsr     updateAudioWaitForNmiAndResetOamStaging
        jmp     gameMode_levelMenu_processPlayer1Navigation

player2PressStartPatch:
        .byte   $20,$A4,$18
        .byte   $FF,$FF,$FF,$FF,$FF
        .byte   $19,$02,$FF ; P2
        .byte   $19,$1B,$0E,$1C,$1C,$FF ; PRESS
        .byte   $1C,$1D,$0A,$1B,$1D,$52 ; START!
        .byte   $FF,$FF,$FF,$FF
        .byte   $FF
player1ActivePatch:
        .byte   $20,$A4,$18
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $19,$15,$0A,$22,$0E,$1B,$FF,$01
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF
player2ActivePatch:
        .byte   $20,$A4,$18
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $19,$15,$0A,$22,$0E,$1B,$FF,$02
        .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF

gameMode_levelMenu_processPlayer2Navigation:
        .export gameMode_levelMenu_processPlayer2Navigation
        lda     numberOfPlayers
        cmp     #$01
        bne     @twoPlayers
        inc     gameMode
        rts

@twoPlayers:
        jsr     updateAudioWaitForNmiAndDisablePpuRendering
        jsr     disableNmi
        jsr     bulkCopyToPpu
        .addr   player2ActivePatch
        jsr     waitForVBlankAndEnableNmi
        jsr     updateAudioWaitForNmiAndEnablePpuRendering

@afterPatch:
        lda     #$00
        sta     activePlayer
        lda     player2_startLevel
        sta     startLevel
        lda     player2_startHeight
        sta     startHeight
        lda     originalY
        sta     selectingLevelOrHeight
        lda     newlyPressedButtons_player2
        sta     newlyPressedButtons
        jsr     gameMode_levelMenu_handleLevelHeightNavigation
        lda     startLevel
        sta     player2_startLevel
        lda     startHeight
        sta     player2_startHeight
        lda     selectingLevelOrHeight
        sta     originalY
        lda     newlyPressedButtons_player2
        cmp     #$10
        bne     @checkBPressed
        lda     heldButtons_player2
        cmp     #$90
        bne     @startAndANotPressed
        lda     player2_startLevel
        clc
        adc     #$0A
        sta     player2_startLevel
@startAndANotPressed:
        lda     #$00
        sta     gameModeState
        lda     #$02
        sta     soundEffectSlot1Init
        inc     gameMode
        rts

@checkBPressed:
        lda     newlyPressedButtons_player2
        cmp     #$40
        bne     @doneProcessing
        lda     #$02
        sta     soundEffectSlot1Init
        jsr     updateAudioWaitForNmiAndResetOamStaging
        jmp     gameMode_levelMenu

@doneProcessing:
        jsr     updateAudioWaitForNmiAndResetOamStaging
        jmp     @afterPatch


gameModeState_handleGameOver_mod:
        .export gameModeState_handleGameOver_mod
        lda     numberOfPlayers
        cmp     #$01
        bne     @twoPlayers
        jmp     gameModeState_handleGameOver

@twoPlayers:
        lda     player1_playState
        ora     player2_playState
        beq     @gameOver
        ; put known data in a, to avoid it from matching "cmp gameModeState" in
        ; @mainLoop. In 1 player mode, numberOfPlayers will be in a.
        lda     #$00
        inc     gameModeState
        rts

@gameOver:
        jmp     gameModeState_handleGameOver


updateMusicSpeed_noBlockInRow_mod:
        .export updateMusicSpeed_noBlockInRow_mod
        tax
        and     activePlayer
        eor     allegro
        sta     allegro
        txa
        cmp     activePlayer
        rts

updateMusicSpeed_foundBlockInRow_mod:
        .export updateMusicSpeed_foundBlockInRow_mod
        tax
        ora     activePlayer
        sta     allegro
        txa
        cmp     #$00
        rts

playState_updateGameOverCurtain_curtainFinished_mod:
        .export playState_updateGameOverCurtain_curtainFinished_mod
        sta     playState
        sta     newlyPressedButtons_player1

        lda     numberOfPlayers
        cmp     #$02
        bne     @ret

        ; playState has not yet been copied to player*_playState
        lda     activePlayer
        cmp     #$01
        bne     @playerTwoActive
        lda     player2_playState
        beq     @ret
        bne     @resumeMusic
@playerTwoActive:
        lda     player1_playState
        beq     @ret

@resumeMusic:
        jsr     updateMusicSpeed_playerDied

@ret:
        rts

updateMusicSpeed_playerDied:
        lda     allegro
        and     activePlayer
        eor     allegro
        sta     allegro
        bne     @fast

        ldx     musicType
        lda     musicSelectionTable,x
        jsr     setMusicTrack
        rts

@fast:
        lda     musicType
        clc
        adc     #$04
        tax
        lda     musicSelectionTable,x
        jsr     setMusicTrack
        rts
