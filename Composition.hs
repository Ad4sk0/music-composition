import Euterpea
import System.Random

-- linux timidity -iA -Os
-- In order to generate different music change value of random generators and play with options

-- RANDOM SEEDS
seed1 = mkStdGen 2
seed2 = mkStdGen 4
seed3 = mkStdGen 9
seed4 = mkStdGen 8
seed5 = mkStdGen 10
seed6 = mkStdGen 12
seed7 = mkStdGen 14

-- OPTIONS
chords_vol = 45
bass_vol = 70
solo_vol = 90

chords_len = qn
bass_len = qn
solo_len = qn

solo_lowest_octave = 4
solo_highest_octave = 6

--- FUNCTIONS
generate_scale intervals key o1 o2 = 
               let int = cycle intervals
                   x = [s + sum (take i int) | (i,x) <- zip [0..127] int, s + sum (take i int) < 127]
                   y = filter (\x -> div x 12 >= o1 && div x 12 <= o2) x
                   s | key == "C" = 0
                     | key == "C#" = 1
                     | key == "D" = 2
                     | key == "D#" = 3
                     | key == "E" = 4
                     | key == "F" = 5
                     | key == "F#" = 6
                     | key == "G" = 7
                     | key == "G#" = 8
                     | key == "A" = 9
                     | key == "A#" = 10
                     | key == "B" = 11   
               in y

choose_random :: [a] -> StdGen -> (a, StdGen)
choose_random [] g = error "Nie ma co wybrac"
choose_random xs g = 
              let (i, g') = next g
              in (xs !! (mod i (length xs)), g')
              
choose_similar xs idx g = 
                           let (temp_i, g') = randomR (idx - 3, idx + 3) g
                               i | temp_i < 0 = 0
                                 | temp_i > length xs - 1 = length xs - 1
                                 | otherwise = temp_i
                           in (xs !! i, i, g')
                           
generate_melody_sim :: Int -> [Int] -> Int -> StdGen -> Music (AbsPitch, Volume)
generate_melody_sim last scale v g = 
  let (pitch, idx, g0) = choose_similar scale last g
      (len, g1) = choose_random [qn, en, hn] g0
      (vol, g2) = choose_random [40..100] g1
      x | vol < 40 = rest len
        | vol >= 40 = note len (pitch, vol)
      y | v == 0 = x
        | otherwise = x :+: generate_melody_sim idx scale (v-1) g2
  in y



makeChord notes len vol = chord (map (note len) (zip notes (cycle [vol])) )

toInt :: Float -> Int
toInt x = round x




--- 1. CREATE RYTHM - Waltz rythm
rythm :: Music (AbsPitch, Volume) -> Music (AbsPitch, Volume) -> Music (AbsPitch, Volume)
rythm b chord = line[b, chord, chord]


--- 2. CREATE CHORDS
c_maj_chord = makeChord [48, 52, 55] chords_len chords_vol  -- C E G
c_maj_bass = note bass_len (36, bass_vol)
c_maj_solo = line [note (1/8) (52, solo_vol), note (1/8) (53, solo_vol), note qn (52, solo_vol)]
c_maj = rythm c_maj_bass c_maj_chord
c_maj_s = chord[c_maj, c_maj_solo]

e_min_7_chord = makeChord [47, 50, 52, 55] chords_len chords_vol -- B D E G
e_min_7_bass = note bass_len (40, bass_vol)
e_min_7_solo = line [note (1/8) (52, solo_vol), note (1/8) (53, solo_vol), note qn (52, solo_vol)]
e_min_7 = rythm e_min_7_bass  e_min_7_chord
e_min_7_s = chord[e_min_7, e_min_7_solo]

a_min_chord = makeChord [45, 48, 52] chords_len chords_vol -- A C E
a_min_bass = note bass_len (45, bass_vol)
a_min_solo = line [note (3/8) (52, solo_vol), note (1/8) (53, solo_vol), note qn (52, solo_vol)]
a_min = rythm a_min_bass  a_min_chord
a_min_s = chord[a_min, a_min_solo]

f_maj_chord = makeChord [45, 48, 53] chords_len chords_vol-- A C F
f_maj_bass = note bass_len (41, bass_vol)
f_maj_solo = line (map (note (1/8)) (zip [52, 53, 55, 53, 52, 53] (cycle [solo_vol])))
f_maj = rythm f_maj_bass  f_maj_chord
f_maj_s = chord[f_maj, f_maj_solo]
f_maj_bass_only = note (bass_len * 3) (41, bass_vol)



--- 3. CREATE SCALE
neapolitan_min_scale = [1, 2, 2, 2, 1, 3 ,1]
neapolitan_min_scale_s = [1, 2, 2, 3, 4]
phrigian_scale = [1,2,2,2,1,2,2]
scale = generate_scale phrigian_scale "E" solo_lowest_octave solo_highest_octave



-- 4. CREATE RANDOM MELODIES
piano_verses_number = 8
first_impro_len = 38
second_impro_len = 55
third_impro_len = 18

melody1 = generate_melody_sim 52 scale first_impro_len  seed1
melody2 = generate_melody_sim 44 scale second_impro_len seed2
melody3 = generate_melody_sim 70 scale third_impro_len  seed3
melody4 = generate_melody_sim 50 scale third_impro_len  seed4
melody5 = generate_melody_sim 70 scale third_impro_len  seed5
melody6 = generate_melody_sim 60 scale third_impro_len  seed6
melody7 = generate_melody_sim 60 scale third_impro_len  seed7
melody8 = generate_melody_sim 60 scale 15  seed7


-- 5. CREATE COMPOSITION
intro = line [
  c_maj,
  e_min_7,
  a_min,
  f_maj,
  c_maj,
  e_min_7,
  a_min,
  f_maj_bass_only]


main_theme = line (take (piano_verses_number * 2) (cycle [
  c_maj,
  e_min_7,
  a_min,
  f_maj]))

main_theme_with_solo = line (take piano_verses_number (cycle [
  c_maj_s,
  e_min_7_s,
  a_min_s,
  f_maj_s]))

chorus = chord [
	main_theme_with_solo
	--instrument Percussion perc_beat1
	]

random_solo = chord [
	main_theme, 
	melody1
	]

random_solo2 = chord [
	line [main_theme, intro],
	instrument Violin melody2
	]

random_solo3 = line [
	chord [
	  main_theme_with_solo,
	  instrument Harmonica melody6,
	  instrument Violin melody7,
	  melody4
	],
	chord [
	  main_theme_with_solo,
	  instrument Violin melody3,
	  instrument Violin melody5,
	  melody7
	]
	]
	
ending = chord [
  intro,
  melody8
  ]

final_song = line [intro, chorus, random_solo, chorus, random_solo2, chorus, random_solo3, ending]

