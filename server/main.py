from flask import Flask, request
from prediction import predict
app = Flask(__name__)


@app.route("/")
def main():
    return "Argo"


@app.route("/healthcheck")
def health_check():
    return "Ok."


@app.route("/api/prediction/<string:project_id>")
def prediction(project_id):    
    flow = request.args.get('flow', None)
    day = request.args.get('day', None)
    if flow is None:
        return "Error: flow parameter is empty"
    if day is None:
        return "Error: day parameter is empty"
    csv = predict()
    return csv


if __name__ == "__main__": 
    app.run(host="0.0.0.0", port=8080)

