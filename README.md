# Galax

<img src="/galaxbw.jpg" width="600">

A "zero-player game"[^1] simulating stellar migration.

[^1]: A zero-player game is a game that evolves without any input from the player. https://en.wikipedia.org/wiki/Zero-player_game

Kinda sorta based on https://arxiv.org/abs/1902.04450.

WIP animations here:
- https://asciinema.org/a/272201
- https://asciinema.org/a/272410
- https://asciinema.org/a/273498
- https://asciinema.org/a/vMDsD4jQN0tpuJMAuOrBjp6LZ

# Running from Docker

```
docker run --rm -it eigenhombre/galax
```

# Running from Source

## Prerequisites

-- `sbcl`
-- `make`
-- Quicklisp

```
$ make
# ...

$ ./galax


Forty-five thousand two hundred sixteen planets surround 10000 stars.

Time 1 kyr; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 2 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 3 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 4 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 5 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 6 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 7 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 8 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 9 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 10 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 20 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 40 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
Time 80 kyrs; 0/45216 planets have life; no planets have intelligent life; no probes sent.
In the wet mountaintops of planet Chri (Ell-I), gaseous carbon chains have begun self-replicating!
In the wet valleys of planet Her (Ssnda-IV), gaseous complex molecules have begun self-replicating!
Time 160 kyrs; 2/45216 planets have life; no planets have intelligent life; no probes sent.
Time 320 kyrs; 2/45216 planets have life; no planets have intelligent life; no probes sent.
Time 640 kyrs; 2/45216 planets have life; no planets have intelligent life; no probes sent.
Greyish-yellow carbon chains have evolved in the scorched valleys of planet Arol (Ali-IV)!
In the lava-filled mountaintops of planet Kathmari (Kat-V), metallic complex molecules have evolved!
On Her (Ssnda-IV), life forms have become conscious!
Life has become conscious on Arol (Ali-IV)!
Slimy algae have started reproducing in the lava-filled valleys of planet Tha (Mel-III)!
On Tha (Mel-III), life has become intelligent!
Time 1280 kyrs; 5/45216 planets have life; 3 planets have intelligent life; no probes sent.
Living beings have become conscious on Chri (Ell-I)!
On Kathmari (Kat-V), life has become intelligent!
Time 1500 kyrs; 5/45216 planets have life; 5 planets have intelligent life; no probes sent.
Time 1800 kyrs; 5/45216 planets have life; 5 planets have intelligent life; no probes sent.
sVitreous nanobes have started reproducing in the smoky valleys of planet Rina (Ida-II)!
Her (Ssnda-IV) is ready to send its first probe!
Launching probe VAKAANDA113 from planet Her (ssnda-iv) to nearest star, Lan!

$
```
