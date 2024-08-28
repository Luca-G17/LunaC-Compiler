import requests
import csv
from html.parser import HTMLParser
from functools import reduce

URL = "https://stationeers-wiki.com/ItemHash"
CSV_FILENAME = "item_hashes.csv"

def pascal_case_to_all_caps(string):
    word_starts = [i for i in range(0, len(string)) if string[i] < 'a']
    word_starts.append(len(string))
    string = string.upper();

    words = [string[word_starts[i]:word_starts[i+1]] for i in range(len(word_starts) - 1)]
    return reduce(lambda a, b: a + '_' + b, words)

def get_item_hashes_from_url():
    response = requests.get(URL).text
    text_ptr = response.find('<tr>', 0)
    with open("item_enum.txt", 'w', newline='') as text_file:
        while True:
            next_ptr = response.find('<tr>', text_ptr + 1)
            if next_ptr == -1:
                break

            table_row = response[text_ptr:next_ptr]
            if not ("/th" in table_row):
                table_row = table_row.split(">")
                item_name = table_row[2].removesuffix(" </td").strip()
                item_hash = table_row[4].removesuffix(" </td").strip()
                item_decimal_hash = table_row[6].removesuffix("</td").strip("\n").strip()
                print(f"{item_name: <50}| {item_hash: <10} | {item_decimal_hash: <10}")
                text_file.write(f"{pascal_case_to_all_caps(item_name)},\n")
            text_ptr = next_ptr

get_item_hashes_from_url()
