
#### verb-forms.

Программа для генерации файлов со спряжениями глаголов. Для работы нужна
[таблица с формами глаголов][1] и конфигурационный файл [verb-forms.yaml][2] с
определением какие формы генерировать. В результате получаются два файла:
`formsQ` - с вопросами, которые всегда записываются азбукой (кроме случаев,
когда глаголу присвоен тег `kanji` в [таблице глаголов][1], и `formsA` - с
ответами, которые всегда записываются иероглифами (кроме тех случаев, когда в
[таблице глаголов][1] нету записи иероглифом).

Файл с вопросами можно читать (или слушать любой text-to-speech программой,
например, [T2S][3]) и записывать ответы, а потом сравнить их с файлом с
ответами.

#### k2csv-exe

....

#### random-nums

....


### Готовые бинарники.

....


### Сборка.

##### Под linux.

1. Установить [haskell stack][4], следуя инструкциям по ссылке или вручную:

    1. На странице с [общими инструкциями][5] скачать архив в разделе [Manual
       download][6] и распаковать.

    2. Скопировать исполняемый файл `stack` в какую-нибудь папку в `PATH`.
       Например, в `~/bin`, если этот путь есть у вас в `PATH`, или в
       `/usr/local/bin`.

2. Запустить в терминале в корневой папке (`jp-k2csv`)

        $ stack build

3. Скачать файл [conjugations.txt][8] (в ту же папку, где находится папка
   `jp-k2csv` (те _не_ в саму папку `jp-k2csv`).

4. Теперь `verb-forms` можно запустить без установки (из папки `jp-k2csv`):

        $ stack exec verb-forms

5. _(не обязательно)_ Для установки нужно выполнить в терминале (из папки
   `jp-k2csv`):

        $ stack install k2csv:verb-forms

    или просто `stack install`, который установит всё (здесь есть ещё
    несколько программ, полный список можно посмотреть в списке `executables`
    в [package.yaml][7]; на данный момент это ещё `k2csv-exe` и
    `random-nums`).

    Если команда запускается от пользователя, то программа будет установлена в
    домашней директории пользователя в `~/.local/bin/`.


#### Под windows.

....


[1]: https://github.com/sgf-dma/jp-conj
[2]: verb-forms.yaml
[3]: https://play.google.com/store/apps/details?id=hesoft.T2S
[4]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[5]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux
[6]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download_2
[7]: package.yaml
[8]: https://raw.githubusercontent.com/sgf-dma/jp-conj/dev/conjugations.txt

