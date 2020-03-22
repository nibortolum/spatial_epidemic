# spatial_epidemic

Companion code for the video https://youtu.be/eCiyCI3s9qc


pour générer les vidéos :
ffmpeg -r 60 -f image2 -s 1920x1080 -i input%03d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test.mp4
