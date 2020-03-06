"""
Process and convert audio files

The following functions are defined:

sph_to_wav(filename_in, folder_out, split_channels, sph2pipe, htype, ext)
mono_to_stereo(channel_1, channel_2, output)
stereo_to_mono(filename, output_folder)
change_byterate(filename, filename_out, end_br)
wav_to_pcm(filename, ext)
pcm_to_wav(filename, nchannels, fs, br)

Author: Pavel Kholiavin
"""

import wavio
import numpy as np
from subprocess import CalledProcessError
from subprocess import check_output
import os.path


def sph_to_wav(filename_in, folder_out, split_channels=False, sph2pipe=r"sph2pipe_v2.5\sph2pipe.exe", htype="rif", ext=".wav", tolin=False):
    """
    Convert .sph to .wav using sph2pipe v2.5
    (can be dl from https://www.ldc.upenn.edu/language-resources/tools/sphere-conversion-tools)

    :param filename_in: .sph file to be converted
    :param folder_out: output folder
    :param split_channels: if True, write two channels in separate files (with _1 and _2 postfixes), False by default
    (if the input file is mono, resulting files will be identical)
    :param sph2pipe: path to sph2pipe.exe
    :param htype: audio header type, rif by default
    :param ext: file extension, .wav by default
    :param tolin: if True, force conversion to 16-bit linear coding
    :return: 1 if fail, 0 if success
    """


    filename_out = os.path.splitext(os.path.split(filename_in)[-1])
    filename_out = os.path.join(folder_out, filename_out[0])

    if split_channels:
        filename_out_1 = filename_out + "_1" + ext
        filename_out_2 = filename_out + "_2" + ext

        command_1 = sph2pipe + " -c 1 -f " + htype + " \"" + filename_in + "\" \"" + filename_out_1 + "\""
        command_2 = sph2pipe + " -c 2 -f " + htype + " \"" + filename_in + "\" \"" + filename_out_2 + "\""

        if tolin:
            command_1 = sph2pipe + " -c 1 -p -f " + htype + " \"" + filename_in + "\" \"" + filename_out_1 + "\""
            command_2 = sph2pipe + " -c 2 -p -f " + htype + " \"" + filename_in + "\" \"" + filename_out_2 + "\""

        try:
            check_output(command_1)
            check_output(command_2)
        except CalledProcessError:
            print("sph2pipe error")
            return 1
        return 0
    else:
        filename_out = filename_out + ext

        command = sph2pipe + " -f " + htype + " \"" + filename_in + "\" \"" + filename_out + "\""
        if tolin:
            command = sph2pipe + " -p -f " + htype + " \"" + filename_in + "\" \"" + filename_out + "\""
        try:
            check_output(command)

        except CalledProcessError:
            print("sph2pipe error")
            return 1
        return 0


def mono_to_stereo(channel_1, channel_2, output):
    """
    merge two separate mono files as two channels in a stereo file

    :param channel_1: file with channel 1
    :param channel_2: file with channel 2
    :param output: output file
    :return: 1 if fail, 0 if success
    """
    try:
        audio_1 = wavio.read(channel_1)
    except FileNotFoundError:
        print("No such file: " + channel_1)
        return 1

    fs_1 = audio_1.rate
    br_1 = audio_1.sampwidth
    data_1 = audio_1.data

    try:
        audio_2 = wavio.read(channel_2)
    except FileNotFoundError:
        print("No such file: " + channel_2)
        return 1

    fs_2 = audio_2.rate
    br_2 = audio_2.sampwidth
    data_2 = audio_2.data

    if fs_1 != fs_2:
        print("Error! Sampling frequency mismatch")
        return 1

    if br_1 != br_2:
        print("Warning! Byterate mismatch; using Br from channel 1")

    data = np.hstack((data_1, data_2))
    wavio.write(output, data, fs_1, sampwidth=br_1)

    return 0


def stereo_to_mono(filename, output_folder):
    """
    split a stereo file into two mono files: same name with _1 and _2 postfixes

    :param filename: file to be split
    :param output_folder: output folder
    :return: 1 if fail, 0 if success
    """
    try:
        audio = wavio.read(filename)
    except FileNotFoundError:
        print("No such file: " + filename)
        return 1

    fs = audio.rate
    br = audio.sampwidth
    data = audio.data

    filename_1 = os.path.splitext(os.path.split(filename)[-1])[0]
    filename_1 = os.path.join(output_folder, filename_1 + "_1.wav")
    filename_2 = os.path.splitext(os.path.split(filename)[-1])[0]
    filename_2 = os.path.join(output_folder, filename_2 + "_2.wav")

    wavio.write(filename_1, data[:, 0], fs, sampwidth=br)
    wavio.write(filename_2, data[:, 1], fs, sampwidth=br)

    return 0


def change_byterate(filename, filename_out, end_br):
    """
    change byterate of a wav file (1 to 4)

    :param filename: file to be processed
    :param filename_out: output filename
    :param end_br: resulting byterate (1, 2, 3 or 4)
    :return: 1 if fail, 0 if success
    """
    try:
        audio = wavio.read(filename)
    except FileNotFoundError:
        print("No such file: " + filename)
        return 1

    fs = audio.rate
    data = audio.data

    print(data[0].dtype)

    try:
        wavio.write(filename_out, data, fs, sampwidth=end_br)
    except ValueError:
        print("Byterate must be 1, 2, 3 or 4")
        return 1
    return 0


def wav_to_pcm(filename, ext=".sbl"):
    """
    convert wav files to raw sbl (pcm)
    :param filename: file to be converted
    :param ext: file extension
    :return: 1 if fail, 0 if success
    """
    try:
        audio = wavio.read(filename)
    except FileNotFoundError:
        print("No such file: " + filename)
        return 1

    data = audio.data
    br = audio.sampwidth

    data_packed = wavio._array2wav(data, br)
    # так, видимо, делать плохо, но если код уже написан до меня...
    filename = os.path.splitext(filename)[0] + ext
    with open(filename, "wb") as f:
        f.write(data_packed)
    return 0


def pcm_to_wav(filename, nchannels, fs, br):
    """
    convert raw pcm/sbl to wav
    :param filename: file to be converted
    :param nchannels: number of channels
    :param fs: sampling frequency in Hz
    :param br: byterate (1, 2, 3 or 4)
    :return: 1 if fail, 0 if success
    """
    try:
        with open(filename, "rb") as f:
            data_packed = f.read()
    except IOError:
        print("No such file: " + filename)
        return 1

    data = wavio._wav2array(nchannels, br, data_packed)

    filename = os.path.splitext(filename)[0] + ".wav"
    wavio.write(filename, data, fs, sampwidth=br)
    return 0


def main():
    # testing
    # change_byterate("C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01.wav", "C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_8.wav", 1)
    # change_byterate("C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01.wav", "C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_16.wav", 2)
    # change_byterate("C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01.wav", "C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_32.wav", 4)
    change_byterate("C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_8.wav", "C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_8-16.wav", 2)

    # change_byterate("C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_snip.wav", "C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_snip_8.wav", 1)
    # change_byterate("C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_snip_8.wav", "C:\\Users\\User\\Downloads\\wavtest\\d61_s14_01_snip_24.wav", 3)
    # folder = r"F:\2002 Rich Transcription Broadcast News and Conversational Telephone Speech\audio\eval02\english\cts"
    # filename_in = "sw_30016.sph"
    # sph_to_wav(os.path.join(folder, filename_in), folder)


if __name__ == '__main__':
    main()



