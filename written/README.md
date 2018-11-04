Eine LaTex-Vorlage, gedacht für Studienarbeiten an der Zeppelin Universität.

Zur Struktur: zentrales Dokument ist main.tex, hier wird das gesamte Dokument kompiliert. Der Text selbst liegt allerdings in einzelnen Teilen (Empfehlung: eine Datei pro Kapitel oder Abschnitt) im Ordner tex und wird über den Befehl \input in  main.tex eingebunden. Vorteil: man kann sich beim Schreiben wirklich auf den Text konzentrieren und damit die Vorteile von LaTex voll nutzen. Alle Pakete liegen in der Datei mystyle.sty, dort können auch neue Pakete eingebunden werden. Abbildungen und Tabellen werden (respektive) in den Ordnern fig und tab abgelegt.

Wer direkt loslegen will: für jeden Teil ein neues .tex-file im Ordner "tex" erstellen, per \input in das main.tex-Dokument einbauen, und im Teil loslegen. Details zu \input und \include finden sich hier: https://en.wikibooks.org/wiki/LaTeX/Modular_Documents.
