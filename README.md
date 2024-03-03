# MM2Conv

This is a program which converts [TrackerBoy TBM](https://www.trackerboy.org/) modules into a format usable by the [Rabbit GB sound driver.](https://www.vgmpf.com/Wiki/index.php?title=Giraffe_Soft_(Sound_Driver))

# Usage

```sh
mm2conv -i input.tbm -o output.asm --ticks=6 --title="..." --timer='$BB' --poly="1,2,3,..."
```

`--ticks` controls how many ticks each row should take.

Refer to `RMW2_Sequence_Fun_v2.txt` for information about `--timer`.

`--poly` is a predefined set of noise channel polynomials to use. This is useful if composing multiple songs for one soundtrack or romhack. If not specified, the converter will create its own list and display it when finished.
