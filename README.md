# Probeklausur 2

Die Probeklausur entspricht im Umfang und Schwierigkeitsgrad der eigentlichen 
Klausur dieses Semesters. 

Es gibt 4 Aufgaben, die Sie lösen sollen.

Im Gegensatz zur ersten Probeklausur gibt es nun keinen "R" Ordner mehr, der die Aufgaben enthält. 
Stattdessen enthält die Datei **`exam.Rmd`** sämtliche Aufgaben und damit die gesamte Klausur. 
Bitte lösen Sie die Aufgaben direkt in dieser Datei, indem Sie den fehlenden Code ergänzen.

Im Ordner `other_formats` finden Sie die Aufgaben auch noch in anderen Formaten (.pdf und .md) 
und jeweils auf Deutsch oder Englisch. Dies soll die Lesbarkeit/Arbeit mit den Aufgaben 
erleichtern. 
**Es ist jedoch ausschließlich die Datei `exam.Rmd` im Hauptordner zu bearbeiten!**

### Bearbeiten der Aufgaben

Schauen Sie sich die Aufgaben in Ruhe an. Diese sind nach 
steigender Schwierigkeit und in verschiedene Themen unterteilt, die Ihnen 
Hinweise darauf geben, wo Sie im Material nach Unterstützung suchen sollten. 
Zu jeder Aufgabe finden Sie leere Funktionen, die mit Überschriften wie 
*"Schreiben Sie eine Funktion, die X macht"* versehen sind. Füllen Sie den 
Funktionskörper (d. h. den Teil zwischen den geschweiften Klammern `{ }`) mit 
Code, der das tut, was verlangt wird. Beispiel:

```r
# Schreiben Sie eine Funktion, die x durch y teilt
ex01Divide <- function(x, y) {
# TODO
}
```

sollte so aussehen:

```r
# Schreiben Sie eine Funktion, die x durch y teilt
ex01Divide <- function(x, y) {
x / y
}
```

(Denken Sie daran, dass eine Funktion immer das Ergebnis des letzten ausgeführten Befehls zurückgibt.)

In der Prüfung sollen nur R Standardpakete und die im Kurs behandelten Pakete benutzt werden.

**Die Bibliotheken werden automatisch geladen, daher brauchen Sie diese nicht 
mit `library()` in Ihrem Code laden.** 

Bitte verwenden Sie weder `require(`, `library(` noch `::` / `:::`, um andere 
Bibliotheken zu laden, und installieren Sie keine weiteren Pakete für diese Aufgabe. 
Darüber hinaus handelt es sich um einen `R`-Kurs, daher führen Sie keine externe 
Software aus Ihrem Code aus.

### Automatisierte Tests

Die Tests befinden sich nun direkt unter jeder Aufgabe. Um zu prüfen, ob ein Test 
bestanden ist, können Sie entweder den entsprechenden Chunk ausführen, indem Sie 
oben rechts auf das grüne "Play"-Symbol klicken, oder die Code-Zeile für den Test 
markieren und ausführen. 

Ist ein Test bestanden, so erscheint unter dem Code Chunk 
in grün "Test passed". Andernfalls ist der Test nicht bestanden und gibt nicht die 
volle Punktzahl.

Aufgabe 01 hat keine Tests.  

Aufgabe 03 wird nur in Bezug auf die Statistiken der zu erstellenden Objekte getestet.  
Das bedeutet, dass diese Tests teilblind sind. 

Ganz am Ende der `exam.Rmd` Datei befindet sich der Code, um sämtliche Tests für 
die gesamte Klausur auszuführen. 
Sie können jederzeit in die Konsole den folgenden Befehl eingeben, um alle 
Tests laufen zu lassen:

```sh
EXERCISES <- character(0); source("evaluate_submission.R")
```

Hinweis: Um sicherzustellen, dass Sie das gleiche Bewertungswerkzeug verwenden 
wie wir, **ist es wichtig, dass Sie die Dateien `convert_submission`, 
`evaluate_submission.R` und `evaluate_test.R` NICHT ändern und den Inhalt 
des `.github`-Ordners nicht verändern!** Wenn Sie diese 
Dateien bearbeiten, ist es möglich, dass Ihre Abgabe Fehler enthält, die das 
Skript jedoch nicht anzeigt.
