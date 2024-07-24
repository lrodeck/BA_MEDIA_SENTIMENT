# BACHELOR ARBEIT REPO
# Quantitative Zeitreihenanalyse der Medienberichterstattung und dem Wandel der Öffentlichen Stimmung zur Flüchtlingskrise 2015/2016
## Automatisierte Textanalyse und Medieneinfluss im Fokus
Diese Hausarbeit untersucht die Entwicklung des Sentiments in derBerichterstattung über 
Flüchtlinge in Deutschland während der Flüchtlingskrise 2015/2016. Die Analyse konzentriert
sich auf die ‘Wir schaffen das!’-Rede von Angela Merkel am 31. August 2015 und die
Silvesternacht 2015/2016. Mittels Sentimentanalyse und Themenclustering werden Artikel aus
den Online-Zeitungen Bild Online und Spiegel Online verglichen. Die Ergebnisse zeigen, dass
das Sentiment zu Beginn positiv war und sich im Laufe der Zeit negativ veränderte, wobei
beide Zeitungen diese Entwicklung widerspiegelten. Die Studie bietet Einblicke in die Rolle der
Medien bei der Wahrnehmung der Flüchtlingskrise. Die Verwendung von Natural Language
AI-Modellen ermöglicht eine umfassende Analyse großer Textmengen. Die Ergebnisse tragen zur
Reflexion über den Medieneinfluss auf die öffentliche Meinung bei und bieten eine Grundlage
für zukünftige Forschung über die Berichterstattung zu gesellschaftlich relevanten Themen.



## Sentiment Analyse
Ausgehend von der Birkner u. a. (2020) ausgeführten Zeitreihe definiere ich meinen Untersuchungszeitraum
um diese zwei Events herum. Er beginnt Juli 2015 und endet im März 2016. Es umfasst damit die
9
beiden relevanten Ereignisse, den ‘Wir schaffen das!’-Auspruch von Angela Merkel Ende August
2015 und die Sylvesternacht in Köln mit genug Nachlauf, um Trendveränderungen erkennen zu
können.
Eine einfache Zeitreihe lässt einen negativen Trend erkennen. Der Sommer 2015 beginnt relativ
positiv. Diese Einstellung nimmt jedoch bis April 2016 ab. Die Bildzeitung macht dabei einen
deutlich stärkeren Wandel durch. SPON, wenn auch mit einem leicht negativen Trend verändert
sich in Sentiment kaum. Teilweise wird hier Haller (2017) bestätigt. Beide Zeitungen erleben
eine wahrnehmbare negative Veränderung. Begann der Trend für die Bildzeitung im beobachteten
Zeitraum bei etwas über 0,15, findet er sich am Ende bei etwas unter 0,1. Über alle Themen hinweg
hat das durchschnittliche Sentiment damit um über 5% abgenommen.

<img width="367" alt="image" src="https://github.com/lrodeck/Hausarbeit-MedSoz-Stru-Pol-Kon-Zus/assets/41971053/69100e0a-56b8-4732-bdbe-6b8faf09f652">


Mittels der Clusterergebnisse lassen sich nun die Sentimente aufgeteilt nach Thema ploten und ihre
individuelle Entwicklung beobachten. Tatsächlich lässt sich erkennen, dass vor allem im Thema
‘Europäische Flüchtlingskrise’ das Sentiment im beobachteten Zeitraum extrem gesunken ist. Diese
Beobachtung gilt für beide beobachteten Medien. Die anderen Themen sind von dieser Entwicklung
weitestgehend unbetroffen. Zwar gibt es auch im Bereich des islamistischen Terrors eine negative
Entwicklung, jedoch nur auf Seiten der Bildzeitung

<img width="490" alt="image" src="https://github.com/lrodeck/Hausarbeit-MedSoz-Stru-Pol-Kon-Zus/assets/41971053/8c160c39-079d-498b-9b4d-e6fa83b8303e">

## Fazit
Diese Hausarbeit hat es sich zum Ziel gemacht, vor dem Hintergrund der Flüchtlingskrise von
2015 bis 2016 mittels eines Natural Language Models die Sentimententwicklung von Artikeln der
beiden Onlineausgaben der Leitmedien SPON und Bild zu untersuchen. Dies sollte dem Zweck
dienen, Erkenntnisse von z.B. Haller (2017) mit anderen Methoden zu überprüfen.
Mittels Topic Modelling wurden die Texte in die 4 überordnenden Themen ‘Rechtsextremismus’,
‘Islamischer Terrorismus’, ‘Europäische Flüchtlingskrise’ und ‘US-Wahl’ eingeteilt. Dabei lässt sich
erkennen, dass der generelle Trend vom Sommer 2015 bis Frühjahr 2016 merklich gesunken ist.
Der Beobachtungszeitraum beginnt im Sommer 2015 mit einer leicht positiven Tendenz, nimmt im
Verlauf des Jahres aber weiterhin ab und ist am Ende des Zeitraums in das leicht negative gekippt.
Überraschend ist, dass sich dieser Trend in beiden Publikationen nahezu identisch wiederfindet.
Diese Ergebnisse sind stimmig mit anderer Forschung wie etwa Haller (2017). Diese fanden ebenfalls
für den Anfangszeitraum eine vor allem positive Stimmung vor und heben dabei besonders das Narrativ der ‘Willkommens Kultur’ hervor (Ebd. S. 99). 
Ebenso beschreiben sie, dass während der Berichterstattung häufig die Positionen der politischen Eliten wiedergegeben wurden und diese
dadurch dem Theorem der Agenda Setting folgend besonders viel Platz im öffentlichen Diskurs
eingeräumt bekamen (Ebd. S. 134). Nachdem wir gezeigt haben, dass das sich wandelnde Sentiment
tatsächlich reproduzierbar zu zeigen ist, könnte dies ein erster Schritt sein, eine Beobachtung wie
der von Entman (2003) beschriebene Kaskadeneffekt nachzuweisen. Haller (2017) beobachtungen
bezüglich der bezogenen Quellen der Leitmedien geben ebenfalls bereits starke Hinweise auf diese
Vermutung.

Weiterhin zeigt diese Arbeit, dass automatisierte Textverarbeitung schnelle und effiziente Studien
mit tausenden Textdokumenten als Datengrundlage deutlich vereinfacht. Gerade unter dem Thema
Open Data kann dies ein entscheidender Fortschritt in der Sozial- und Medienwissenschaftlichen
Forschung sein. Mit Blick auf die Zukunft wäre es allerdings wünschenswert, spezifischere
Modelle zur Verfügung zu haben, deren Trainingssets öffentlich einsehbar sind. Denn obwohl die
Berechnungen des AI-Modells durchaus schlüssig sind, ist es am Ende unmöglich zu interpretieren,
warum das Model zu dem Schluss kam, zu dem es gegkommen ist. Zudem sollten generelle
Standards im Umgang mit schwer interpretierbaren Machine-Learning Modellen gefördert und
genauso gelehrt werden. An dieser Stelle muss von Forschungsseite dringend nachgebessert werden.
Weiterführende Arbeiten könnten sich mit der Analyse der Akteure innerhalb der Artikel, die in
diesem Datenset gesammelt wurden, beschäftigen, um die Erkenntnisse von Haller (2017), dass
die meisten Quellen oftmals aus politischen Eliten stammen, zu überprüfen. Auch dies sollte mit
Machine-Learning-Modellen effizient zu gestalten sein.


# Update

Dank der Verfügbarkeit von neueren Topic Classifiern wie BERTOPIC könnte eine tiefer gehende Analyse in Zukunft noch mals leichter werden. 
Eine erste Analyse zeigt bereits deutlich mehr kleinere Topics innerhalb der Datenmenge. BERTOPIC kommt ebenfalls mit der Möglichkeit diese Topics 2 Dimensional zu mappen:

![newplot (1)](https://github.com/user-attachments/assets/6d4d00e1-8d8f-4993-b88f-3ea974dc485f)
