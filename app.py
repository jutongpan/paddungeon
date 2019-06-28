from flask import Flask, render_template, request, jsonify
import sqlite3
import pandas as pd
import re
import urllib.request

app = Flask(__name__)

df_dungeon = pd.read_csv("dungeon.csv")
list_dungeonName = df_dungeon.dungeonName.tolist()

@app.route('/')
def index():
    return render_template(
        'index.html',
        list_dungeonName = list_dungeonName
        )

@app.route('/<dungeon>', methods=['GET', 'POST'])
def dungeonPage(dungeon):
    if request.method == 'POST':
        return redirect(url_for('index'))

    return render_template(
        'dungeonPage.html',
        dungeonName = dungeon,
        dungeonHtml = 'dungeonHtml/' + dungeon + '.html'
        )

if __name__ == '__main__':
    app.run(debug=True)