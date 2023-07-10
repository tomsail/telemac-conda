CMD = 'mkdir -p ./animation ; rm -f ./animation/*'
system(CMD)

set term png
set yrange [4:11]
set ylabel 'Z (m)' offset 2,0
ml = `grep '^$' 1Dres_Ch1d.plt | wc -l`
do for [i=0:ml-2] {
  outfile = sprintf('animation/Channel%03.0f.png',i)
  set output outfile
  set label sprintf("time = %6.1f",i*20) at 5000,9.5 center font 'helvetica,16'
  plot '1Dres_Ch1d.plt' every :::i::i using 2:3 w l ti '1D stretch Z', '2Dres_Ch2d.plt' every :::i::i using (5000+$2):3 w l lt 2 lw 3 ti '2D channel Z'
  unset label
}

ENCODER = system('which mencoder');
CMD = 'mencoder mf://animation/*.png -mf fps=7:type=png -ovc lavc -lavcopts vcodec=mpeg4:mbd=2:trell -oac copy -o Channel.avi'
system(CMD)
