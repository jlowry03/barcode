// -*- mode: js -*-
const aws       = require('aws-sdk');
const s3        = new aws.S3({ apiVersion: '2006-03-01' });
const dstBucket = "frantically-frugal--barcodes";
const spawnLineByLine = require('aws-lambda-binary').spawnLineByLine;

exports.handler = async (event,context) => {
    // ------------------------------ PROGRAMS ---------------------------------
    const dstKey = `barcode-${event.location}-${event.eventID}-${event.userID}.png`;

    const rktBarcodeGen = spawnLineByLine({
        spawn: {
            command: 'barcode'
            , args: [event.location, event.eventID, event.userID]
        }
    });
    const rktImageGen = spawnLineByLine({
        spawn: {
            command: 'widths-to-ppm'
        }
    });
    const imConvert = spawnLineByLine({
        spawn: {
            command: 'convert'
            , args: [ "PPM:-", "PNG:-" ] // <-- maybe this was the unexpected string
            //so that we can capture the PNG as a string.
        }
    });
    // -------------------------------------------------------------------------
    [rktBarcodeGen, rktImageGen, imConvert]
        .map(x=>x.ensureIsRunning());

    let widths = "";
    rktBarcodeGen.stdin();
    rktBarcodeGen.stdout((result)=>{
        widths = JSON.parse(result);
    });

    console.log("widths:=",JSON.Stringify(widths));
    console.log("barcode:=",widths.barcode.toString());
    console.log("barcode_widths:=",widths.barcode_widths.toString());

    let ppm = "";
    rktImageGen.stdin(JSON.Stringify(widths.barcode_widths));
    rktImageGen.stdout((result)=>{
        ppm = result;
    });

    let png = "";
    imConvert.stdin(ppm);
    imConvert.stdout((result)=>{
        png = result;
    });

    //https://docs.aws.amazon.com/lambda/latest/dg/with-s3-tutorial.html#with-s3-tutorial-create-function-createfunction
    try {
        const destparams = {
            Bucket: dstBucket,
            Key: dstKey,
            Body: png,
            ContentType: "image/png"
        };

        const putResult = await s3.putObject(destparams).promise();
        console.log("PutResult[INFO]:: " + putResult.toString());
    } catch (error) {
        console.log(error);
        return;
    }

    console.log("Successfully generated barcode for" +
                event.location + event.eventID + event.userID
                +  "and uploaded to" +  dstBucket + "/" + dstKey);
};
