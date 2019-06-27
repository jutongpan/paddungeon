from flask import Flask, render_template, request, jsonify
import sqlite3
import pandas as pd
import re
import urllib.request

app = Flask(__name__)

@app.route('/')
def index():
    return render_template(
        'index.html'
        )

@app.route('/<dungeon>', methods=['GET', 'POST'])
def dungeonPage(dungeon):
    if request.method == 'POST':
        return redirect(url_for('index'))

    return render_template('dungeonPage.html', dungeon = 'dungeonHtml/' + dungeon + '.html')

if __name__ == '__main__':
    app.run(debug=True)