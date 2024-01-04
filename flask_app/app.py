from flask import Flask, render_template, jsonify, redirect

import data

app = Flask(__name__)

@app.route("/")
def index():
    return render_template("index.html", data=data)

@app.route("/get/<system>/<station>")
def get(system, station):
    data_retrieved, temp, disox, spc, cdom, ph, disoxsat = data.get_data(str(system), int(station))
    data_retrieved = data_retrieved.to_html()

    return render_template("get.html", d=data_retrieved, temp=temp, disox=disox, spc=spc, cdom=cdom, ph=ph, disoxsat=disoxsat)

@app.route("/means")
def plots():
    return render_template("means.html")

if __name__ == "__main__":
    app.run(debug=True)