# script written by Susanne Fuchs and adapted by Tom Offrede

library(magrittr)
library(readr)
library(tuneR)

# # download function
# dl_from_dropbox <- function(x, key) {
#   bin <- RCurl::getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x),
#                              ssl.verifypeer = FALSE)
#   con <- file(x, open = "wb")
#   writeBin(bin, con)
#   close(con)
# }

# set audio player
# sound::setWavPlayer('/bin/aplay')

### folders
# setwd('C:/Users/fuchs/OneDrive - Leibniz-ZAS/Dokumente/Bradley/vocalisationtrialsaudio/')

#basefolder <- ("C:/Users/fuchs/OneDrive - Leibniz-ZAS/Dokumente/Bradley/")  #what is the current folder?
basefolder <- ("C:/Users/offredet/Documents/1HU/ExperimentEyes/Data/retryFiltering/")
filefolder <- basefolder
alfiles <- list.files(filefolder, pattern = "*.WAV")
statisfolder <- paste0(basefolder, "filtered/")

#Read a Wave File
for(ff in alfiles) #loop through all files
{
  train_audio <- readWave(paste0(filefolder, ff))
  sr <- train_audio@samp.rate
  bit <- train_audio@bit
  par(mar = c(1, 1, 1, 1))
  phonTools::spectrogram(train_audio@left, fs=sr)

# FFT of noise
fftlen <- 1024
noise <- train_audio@left[1:fftlen]

#plot(noise, type='l')
fft0 <- fft(noise)
mag0 <- Mod(fft0)

# plot spectrum of noise
plot(mag0[1:(fftlen/2)], type='l')
hertz <- seq(1, sr/2, length.out=fftlen/2)
plot(hertz, mag0[1:(fftlen/2)], type='l', xlab="Frequency (Hz)", ylab="Amplitude")

# grab a section of audio to demonstrate spectral subtraction
chunk <- train_audio@left[1:2205]
phonTools::spectrogram(chunk, fs=sr, color=F, dynamicrange=65, maxfreq=sr/2)

fft1 <- fft(chunk)
mag1 <- Mod(fft1)
plot(hertz, mag1[1:(fftlen/2)], type='l')

# compare noisy audio and baseline noise
plot(hertz, mag1[1:(fftlen/2)], type='l', col='red')
lines(hertz, mag0[1:(fftlen/2)], col='blue')

# spectral subtraction
mag2 <- mag1 - mag0
plot(hertz, mag2[1:(fftlen/2)], type='l')
abline(a=0, b=0, col='red')
mag2[mag2<0] <- 0
plot(hertz, mag2[1:(fftlen/2)], type='l')

# inverse FFT
phs1  <- Arg(fft1)
j     <- sqrt(as.complex(-1))
dn        <- mag2 * exp(j*phs1)
denoised  <- Re(fft(dn,inverse=T)/fftlen)

# look at cleaned IFFT waveform
phonTools::spectrogram(denoised, fs=sr, color=F, dynamicrange=65, maxfreq=sr/2)

# let's do it for the whole audio signal!
N <- length(train_audio@left)
cleaned <- numeric(length=N)

# overlapping moving window
overlap <- fftlen/2

# Hann window/kernel
hannwin <- ( 0.5 - (0.5 * cos(2*pi*(0:(fftlen-1))/(fftlen-1))) )
hannwin <- phonTools::windowfunc(fftlen, type="hann")
#plot(hannwin, type='l')

# half-Hann kernels
hann1 <- hannwin[(1 + fftlen/2):fftlen]
hann2 <- hannwin[1:(fftlen/2)]

# additive property of two half-Hann kernels
plot(hann1, type='l', col='red')
lines(hann2, col='blue')
lines(hann1 + hann2, col='purple')
chunk <- train_audio@left[2205:4000]

plot(hann1*chunk, type='l', col='red')
lines(hann2*chunk, col='blue')

# how many overlapping windows will fit?
bins <- floor(N/(fftlen-overlap))

# moving window over whole audio signal
for (bin in 1:bins) {
  chunk <- train_audio@left[(1+(bin-1)*(fftlen-overlap)):(fftlen+(bin-1)*(fftlen-overlap))]
  fft1 <- fft(chunk)
  mag1 <- Mod(fft1)
  phs1 <- Arg(fft1)
  mag2 <- mag1 - mag0
  mag2[mag2<0] <- 0
  dn        <- mag2 * exp(j*phs1)
  denoised  <- Re(fft(dn,inverse=T)/fftlen)
  overlap1  <- cleaned[(1+(bin-1)*(fftlen-overlap)):(overlap+(bin-1)*(fftlen-overlap))]
  overlap2  <- denoised[1:overlap]
  cleaned[(1+(bin-1)*(fftlen-overlap)):(overlap+(bin-1)*(fftlen-overlap))] <- overlap1*hann1 + overlap2*hann2
  cleaned[(1+overlap+(bin-1)*(fftlen-overlap)):(fftlen+(bin-1)*(fftlen-overlap))] <- denoised[(1+overlap):fftlen]
}

cleaned[is.na(cleaned)] <- 0

# view spectrograms
phonTools::spectrogram(cleaned, fs=sr, color=T, dynamicrange=65, maxfreq=sr/2)
phonTools::spectrogram(train_audio@left, fs=sr, color=T, dynamicrange=65, maxfreq=sr/2)

# save wavfile 
Wave(cleaned, samp.rate=sr, bit=bit)->nn
nn <- normalize(nn, unit=as.character(bit))
#writeWave(new, ff, extensible = TRUE)
writeWave(nn,paste0(statisfolder, ff))
}

