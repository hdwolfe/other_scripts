#!/usr/bin/perl
#download youtube media

#open bkmrk files
open(input251, "<youtube_bkmrks_251.txt");
open(input22, "<youtube_bkmrks_22.txt");

#parse lines into arrays
@lines251=<input251>;
chomp(@lines251);
@lines22=<input22>;
chomp(@lines22);

#list files already downloaded
system("ls > ls_out");
open(input, "<ls_out");
@out=<input>;
chomp(@out);

#make arrays for each type of downloaded file
@out251=();
@out22=();

#group these files by type of download
foreach $i (@out){

  #sort 251
  if($i=~m/.webm/){
  push @out251, $i;
  }

  #sort 22
  if($i=~m/.mp4/){
  push @out22, $i;
  }
}

#make array file URLs that are not in the file index
@fetch251=();
#@fetch251_pt=();
@fetch22=();
#@fetch22_pt=();

#extract file URLs that are partially downloaded in the file index under 251
#foreach $x (@lines251){
#$id=(split('=',$x))[1];
#
#if(grep /$id/, @out251){
#if(grep /"webm.part"/, @out251){
#push @fetch251_pt, $x;
#}
#}
#}

#extract file URLs that are partially downloaded in the file index under 22
#foreach $x (@lines22){
#$id=(split('=',$x))[1];

#if(grep /$id/, @out22){
#if(grep /"mp4.part"/, @out22){
#push @fetch22_pt, $x;
#}
#}
#}

#extract file URLs that are not in the file index under 251
foreach $x (@lines251){
$id=(split('=',$x))[1];

if(grep /$id/, @out251){
}else{
push @fetch251, $x;
}
}

#extract file URLs that are not in the file index under 22
foreach $x (@lines22){
$id=(split('=',$x))[1];

if(grep /$id/, @out22){
}else{
push @fetch22, $x;
}
}

close(input251);
close(input22);
close(input);

#remove file list index
system("rm ls_out");

#download files
#foreach $i (@fetch251_pt) {
#system("youtube-dl -f 251 $i\n");
#}

#foreach $i (@fetch22_pt) {
#system("youtube-dl -f 22 $i\n");
#}

foreach $i (@fetch251) {
system("youtube-dl -f 251 $i\n");
}

foreach $i (@fetch22) {
system("youtube-dl -f 22 $i\n");
}
