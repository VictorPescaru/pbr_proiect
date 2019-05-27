import sys
import re
#clipspath = r"D:\Info\I3b\Programare bazata pe reguli\Clips\Proiect\facts.clp"
clipspath = r"..\Clips\facts.clp"
def readFile(filename, mode):
    midiFile = open(filename, mode)
    return midiFile


def readHeader(midiFile):
    MThd = midiFile.read(4).decode()
    headerLength = int.from_bytes(midiFile.read(4), "big")
    format = int.from_bytes(midiFile.read(2), "big")
    tracks = int.from_bytes(midiFile.read(2), "big")
    division = int.from_bytes(midiFile.read(2), "big")
    data = {"format": format, "tracks": tracks, "division": division}
    return {"type": MThd, "length": headerLength, "data": data}


def computeDeltaTime(data, i):
    deltaBytes = []
    while data[i][0] == '1':
        deltaBytes.append(data[i][1:])
        i += 1
    deltaBytes.append(data[i][1:])
    deltaBytes = "".join(deltaBytes)
    delta_time = 0
    for j in range(0, len(deltaBytes)):
        delta_time = delta_time + int(deltaBytes[j]) * pow(2, len(deltaBytes) - 1 - j)
    i = i + 1
    return delta_time, i


def computeData(data):
    events = []
    i = 0
    running_status = None
    while i < len(data):
        delta_time, i = computeDeltaTime(data, i)
        eventID = data[i]
        # meta_event
        if eventID == "11111111":
            i = i + 1
            eventCode = data[i]
            i = i + 1
            eventLengthArray = []
            while data[i][0] == "1":
                eventLengthArray.append(data[i][1:])
                i += 1
            eventLengthArray.append(data[i][1:])
            eventLengthArray = "".join(eventLengthArray)
            eventLength = 0
            for j in range(0, len(eventLengthArray)):
                eventLength = eventLength + int(eventLengthArray[j]) * pow(2, len(eventLengthArray) - 1 - j)
            i += 1
            eventData = []
            for j in range(0, eventLength):
                eventData.append(data[i])
                i += 1
            meta_event = {"eventCode": eventCode, "eventLength": eventLength, "eventData": eventData}
            eventType = {"Delta": delta_time, "eventType": meta_event}
            events.append(eventType)
        # sysex_events
        elif eventID == "11110000" or eventID == "11110111":
            eventCode = data[i]
            i += 1
            sysexLengthArray = []
            while data[i][0] == '1':
                sysexLengthArray.append(data[i][1:])
                i += 1
            sysexLengthArray.append(data[i][1:])
            sysexLengthArray = "".join(sysexLengthArray)
            sysexLength = 0
            for j in range(0, len(sysexLengthArray)):
                sysexLength = sysexLength + int(sysexLengthArray[j]) * pow(2, len(sysexLengthArray) - 1 - j)
            sysexData = []
            for j in range(0, sysexLength):
                sysexData.append(data[i])
                i += 1
            sysex_event = {"eventCode": eventCode, "eventLength": sysexLength, "eventData": sysexData}
            eventType = {"Delta": delta_time, "eventType": sysex_event}
            events.append(eventType)
        # midi_events
        elif "1000" <= eventID[0:4] <= "1110":
            running_status = eventID
            eventCode = data[i][0:4]
            eventChannel = int.from_bytes(int("0b" + data[i][4:], 2).to_bytes(1, "big"), "big") + 1
            eventData = {"Channel": eventChannel}
            i += 1
            if eventCode == "1000" or eventCode == "1001" or eventCode == "1010":
                i, eventData = Note(data, int(i), eventData)
            elif eventCode == "1011":
                i, eventData = ControlChange(data, int(i), eventData)
            elif eventCode == "1100":
                i, eventData = ProgramChange(data, int(i), eventData)
            elif eventCode == "1101":
                i, eventData = ChannelKeyPressure(data, int(i), eventData)
            elif eventCode == "1110":
                i, eventData = PitchBend(data, int(i), eventData)

            midi_event = {"eventCode": eventCode, "eventData": eventData}
            eventType = {"Delta": delta_time, "eventType": midi_event}
            events.append(eventType)

        else:
            eventChannel = int.from_bytes(int("0b" + running_status[4:], 2).to_bytes(1, "big"), "big") + 1
            eventData = {"Channel": eventChannel}
            if running_status[:4] == "1000" or running_status[:4] == "1001" or running_status[:4] == "1010":
                i, eventData = Note(data, i, eventData)
            elif running_status[:4] == "1011":
                i, eventData = ControlChange(data, i, eventData)
            elif running_status[:4] == "1100":
                i, eventData = ProgramChange(data, i, eventData)
            elif running_status[:4] == "1101":
                i, eventData = ChannelKeyPressure(data, i, eventData)
            elif running_status[:4] == "1110":
                i, eventData = PitchBend(data, i, eventData)
            midi_event = {"eventCode": running_status[:4], "eventData": eventData}
            eventType = {"Delta": delta_time, "eventType": midi_event}
            events.append(eventType)

    return events


def Note(data, i, eventData):
    eventNote = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    i += 1
    eventVelocity = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    eventData["Note"] = eventNote
    eventData["Velocity"] = eventVelocity
    i += 1
    return i, eventData


def ControlChange(data, i: int, eventData):
    controllerNumber = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    i += 1
    controllerValue = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    eventData["Control"] = controllerNumber
    eventData["Value"] = controllerValue
    i += 1
    return i, eventData


def ProgramChange(data, i: int, eventData):
    newProgramNumber = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    eventData["Number"] = newProgramNumber
    i += 1
    return i, eventData


def ChannelKeyPressure(data, i: int, eventData):
    channelPressureValue = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    eventData["Pressure"] = channelPressureValue
    i += 1
    return i, eventData


def PitchBend(data, i: int, eventData):
    lsb = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    i += 1
    msb = int.from_bytes(int("0b" + data[i], 2).to_bytes(1, "big"), "big")
    eventData["Lsb"] = lsb
    eventData["Msb"] = msb
    i += 1
    return i, eventData


def computeEvent(event):
    # Start meta-events
    eventName = event["eventType"]["eventCode"]
    if eventName == "00000000":
        event["eventType"]["eventCode"] = "SequenceNumber"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big")
        event["eventType"]["eventData"] = int.from_bytes(
            event["eventType"]["eventData"][0] + event["eventType"]["eventData"][1], "big")
    if eventName == "00000001":
        event["eventType"]["eventCode"] = "TextEvent"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00000010":
        event["eventType"]["eventCode"] = "CopyrightNotice"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00000011":
        event["eventType"]["eventCode"] = "TrackName"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00000100":
        event["eventType"]["eventCode"] = "InstrumentName"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00000101":
        event["eventType"]["eventCode"] = "Lyric"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00000110":
        event["eventType"]["eventCode"] = "Marker"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00000111":
        event["eventType"]["eventCode"] = "CuePoint"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big").decode()
        event["eventType"]["eventData"] = "".join(event["eventType"]["eventData"])
    if eventName == "00100000":
        event["eventType"]["eventCode"] = "MIDIChannelPrefix"
        event["eventType"]["eventData"] = int("0b" + event["eventType"]["eventData"], 2). \
            to_bytes(1, "big")
        event["eventType"]["eventData"] = int.from_bytes(event["eventType"]["eventData"], "big")
    if eventName == "00101111":
        event["eventType"]["eventCode"] = \
            "EndOfTrack"
    if eventName == "01010001":
        event["eventType"]["eventCode"] = "SetTempo"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big")
        event["eventType"]["eventData"] = int.from_bytes(
            event["eventType"]["eventData"][0] + event["eventType"]["eventData"][1] + event["eventType"]["eventData"][
                2], "big")
    if eventName == "01010100":
        event["eventType"]["eventCode"] = "SMTPEOffset"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big")
            event["eventType"]["eventData"][i] = int.from_bytes(event["eventType"]["eventData"][i], "big")
        event["eventType"]["eventData"] = {"Hours": event["eventType"]["eventData"][0],
                                           "Minutes": event["eventType"]["eventData"][1],
                                           "Seconds": event["eventType"]["eventData"][2],
                                           "Frames": event["eventType"]["eventData"][3],
                                           "Fractional Frame": event["eventType"]["eventData"][4]}
    if eventName == "01011000":
        event["eventType"]["eventCode"] = "TimeSignature"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big")
            event["eventType"]["eventData"][i] = int.from_bytes(event["eventType"]["eventData"][i], "big")
        event["eventType"]["eventData"] = {"Numerator": event["eventType"]["eventData"][0],
                                           "LogDenomitator": event["eventType"]["eventData"][1],
                                           "MIDIClocksPerMetronomeClick": event["eventType"]["eventData"][2],
                                           "ThirtySecondsPer24Clocks": event["eventType"]["eventData"][3]
                                           }
    if eventName == "01011001":
        event["eventType"]["eventCode"] = "KeySignature"
        for i in range(0, event["eventType"]["eventLength"]):
            event["eventType"]["eventData"][i] = int("0b" + event["eventType"]["eventData"][i], 2). \
                to_bytes(1, "big")
            event["eventType"]["eventData"][i] = int.from_bytes(event["eventType"]["eventData"][i], "big")
        event["eventType"]["eventData"] = {"Sharps or flats": event["eventType"]["eventData"][0],
                                           "Major/Minor Key": event["eventType"]["eventData"][1]
                                           }
    if eventName == "01111111":
        event["eventType"]["eventCode"] = \
            "Sequencer-SpecificMeta-event"
    # End meta-events

    # Start sysex-events
    if eventName == "11110000":
        event["eventType"]["eventCode"] = \
            "F0SysexEvent"
    if eventName == "11110111":
        event["eventType"]["eventCode"] = \
            "F7SysexEvent"
    # End sysex-events

    # Start midi-events
    if eventName == "1000":
        event["eventType"]["eventCode"] = "NoteOff"
    if eventName == "1001":
        event["eventType"]["eventCode"] = "NoteOn"
    if eventName == "1010":
        event["eventType"]["eventCode"] = "PolyphonicKeyPressure"
    if eventName == "1011":
        event["eventType"]["eventCode"] = "ControllerChange"
    if eventName == "1100":
        event["eventType"]["eventCode"] = "ProgramChange"
    if eventName == "1101":
        event["eventType"]["eventCode"] = "ChannelKeyPressure"
    if eventName == "1110":
        event["eventType"]["eventCode"] = "PitchBend"

    return event


def readTracks(midiFile, numberOfTracks):
    tracks = {}
    for i in range(0, numberOfTracks):
        type = midiFile.read(4).decode()
        length = int.from_bytes(midiFile.read(4), "big")
        data = []
        for j in range(0, length):
            z = bin(int.from_bytes(midiFile.read(1), "big")).replace('0b', '').rjust(8, "0")
            data.append(z)
        events = computeData(data)
        for event in events:
            computeEvent(event)

        track = {"type": type, "length": length, "data": events}
        tracks[i] = track
    return tracks


def generateClp():
    global clipspath
    clpfile = open(clipspath, "w")
    templateMidiInfo = \
    '''(deftemplate midiInfo
    (slot midiFormat)
    (slot tracks)
    (slot division)
)
    '''
    templateEvent = \
    '''
(deftemplate event
    (slot order (type INTEGER))
    (slot trackID (type INTEGER))
    (slot delta (type INTEGER))
    (slot tip (type STRING))
    (multislot data)
)
    '''
    deffactsInit = \
'''
(deffacts init
'''
    clpfile.write(templateMidiInfo)
    clpfile.write(templateEvent)
    clpfile.write(deffactsInit)
    with open("midi.txt", "r") as inputFile:
        line = inputFile.readline()
        order = 0
        trackNumber = None
        while line:
            line = re.split("[':{,}\n ]", line)
            while "" in line:
                line.remove("")
            while "eventType" in line:
                line.remove("eventType")
            if line[1] == 'MThd':
                format = line[line.index('format') + 1]
                tracks = line[line.index('tracks') + 1]
                division = line[line.index('division') + 1]
                midiInfoFact = "\t(midiInfo (midiFormat {}) (tracks {}) (division {}))\n\n". \
                    format(format, tracks, division)
                clpfile.write(midiInfoFact)
            elif line[0] == 'TrackNumber':
                trackNumber = line[1]
                order = 0
            else:
                delta = line[line.index('Delta') + 1]
                eventCode = line[line.index('eventCode') + 1]
                eventData = switchEventCode(line, eventCode)
                if eventCode in "NoteOn" and eventData[2] == '0':
                    eventCode = "NoteOff"

                eventData = " ".join(eventData)
                if eventCode in ("TextEvent", "CopyrightNotice", "TrackName",
                                 "InstrumentName", "Lyric", "Marker", "CuePoint"):
                    eventData = "\""+eventData+"\""


                eventCode = "\""+eventCode+"\""
                clpEvent = "\t(event (order {}) (trackID {}) (delta {}) (tip {}) (data {}))\n"\
                    .format(order, trackNumber, delta, eventCode, eventData)
                clpfile.write(clpEvent)
            line = inputFile.readline()
            order += 1
    deffactsInit = \
'''
)
    
'''
    clpfile.write(deffactsInit)


def switchEventCode(line, eventCode):
    eventData = []
    if eventCode in ("TextEvent", "CopyrightNotice", "TrackName",
                     "InstrumentName", "Lyric", "Marker", "CuePoint"):
        index = line.index("eventData") + 1
        data = line[index]
        while index < len(line) - 1:
            index += 1
            data = data + " " + line[index]
        eventData.append(data)
    elif eventCode in ("SequenceNumber", "MIDIChannelPrefix", "SetTempo"):
        eventData.append(line[line.index("eventData") + 1])
    elif eventCode in ("SMTPEOffset", "TimeSignature", "KeySignature", "Sequence-SpecificMeta-Event",
                       "NoteOff", "NoteOn", "PolyphonicKeyPressure", "ControllerChange",
                       "ProgramChange", "ChannelKeyPressure", "PitchBend"):
        index = line.index("eventData") + 2
        eventData.append(line[index])
        while index < len(line) - 1:
            index += 2
            eventData.append(line[index])


    elif eventCode in "EndOfTrack":
        pass

    return eventData


def main():
    outputFile = open("midi.txt", "w")
    midiSource = "MIDI_SAMPLE.mid"
    if len(sys.argv) == 2:
        midiSource = sys.argv[1]
    midiFile = readFile(midiSource, "rb")
    header = readHeader(midiFile)
    print(header, file=outputFile)
    tracks = readTracks(midiFile, header["data"]["tracks"])
    for i in range(0, header["data"]["tracks"]):
        print("TrackNumber:", i, file=outputFile)
        for track in tracks[i]["data"]:
            print(track, file=outputFile)
    generateClp()


if __name__ == "__main__":
    main()
