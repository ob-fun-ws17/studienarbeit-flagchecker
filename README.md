CopyManager

Der CopyManager ist ein FileManager der einzig und allein dazu dient, Dateien zu kopieren. An sich hört sich das nicht besonders an, aber er verfügt noch über ein spezielles Feature, das ihn einzigartig macht: Multicopy.

How to install:
Klone das Repository und builde es mit stack.

Usage:
Der CopyManager wird von der Kommandozeile aus aufgerufen. Danach werden ihm die Befehle eingegeben und er parst sie einzeln. 
Nun zum besten Feature, Multicopy.
Durch das multicopy Kommando kann man eine Datei mehrmal kopieren. Dabei werden die Kopien umbenannt indem sie sich einfach hochzählen. Das gleiche gilt auch für die massMultiCopy option.

Das kann einem vielleicht suspekt vorkommen, hat aber durchaus seinen Nutzen. Wenn man zum Beispiel eine Reihe von Bildern animieren will, mittels ffmpegs image2, dann sind die Kopien schon so benannt, das man sie mittels simplen Regex einlesen kann. So kann man Videos oder Gifs mit höherer Framerate ohne weiteres erstellen. 
Diese Videos und Gifs kann man auch mit niedrigerer Framerate und weniger Duplikaten erstellen, aber dann können manche MediaPlayer, wie beispielsweise VLC, sie nicht mehr abspielen.