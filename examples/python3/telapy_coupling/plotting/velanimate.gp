CMD = 'mkdir -p ./animation ; rm -f ./animation/*'
system(CMD)

set term png
set yrange [4:11]
set y2range [0.:3.5]
set ytics nomirror
set y2tics 0.5
set grid y2tics
set ylabel 'Z (m)' offset 2,0
set y2label 'V (m/s)' offset -2,0
ml = `grep '^$' 1Dres_Ch1d.plt | wc -l`
do for [i=0:ml-2] {
  outfile = sprintf('animation/Channel%03.0f.png',i)
  set output outfile
  set label sprintf("time = %6.1f",i*20) at 5000,9.2 center font 'helvetica,16'
  plot '1Dres_Ch1d.plt' every :::i::i using 2:3 w l ti '1D stretch Z' axis x1y1, '2Dres_Ch2d.plt' every :::i::i using (5000+$2):3 w l lt 2 lw 3 ti '2D channel Z' axis x1y1, '1Dres_Ch1d.plt' every :::i::i using 2:5 w l ti '1D stretch V' axis x1y2, '2Dres_Ch2d.plt' every :::i::i using (5000+$2):4 w l lt 4 lw 3 ti '2D channel V' axis x1y2
  unset label
}

ENCODER = system('which mencoder');
CMD = 'mencoder mf://animation/*.png -mf fps=7:type=png -ovc lavc -lavcopts vcodec=mpeg4:mbd=2:trell -oac copy -o Channelvel.avi'
system(CMD)

