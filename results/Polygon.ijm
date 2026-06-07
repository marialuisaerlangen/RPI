// Global variables
var initialized = false;
var PolygonID = -1;

// Function to set up the table columns
function setupTable() {
    Table.setColumn("Polygon-Nr");
    Table.setColumn("Area");
    Table.setColumn("X");
    Table.setColumn("Y");
}

// Function to draw the label (PolygonID) at the given coordinates
function drawLabel(X, Y, PolygonID) {
    fontSize = 10;
    setFont("SansSerif", fontSize, "bold");
    text = "" + PolygonID;

    textWidth = lengthOf(text) * fontSize * 0.6;
    textHeight = fontSize + 4;

    // small offset so text is not exactly on top of the clicked point
    labelX = X + 4;
    labelY = Y - 4;

    rectX = labelX - 4;
    rectY = labelY - textHeight + 8;

    // white background
    setColor(255, 255, 255);
    makeRectangle(rectX, rectY, textWidth + 8, textHeight);
    run("Fill");

    // black text
    setColor("black");
    drawString(text, labelX, labelY + 8);
}

// Main function
function measureAndDrawPolygon() {
    // Initialize only once
    if (!initialized) {
        PolygonID = parseInt(getString("Start Polygon ID", "1"));
        setupTable();
        initialized = true;
    }

    // Only allow polygon selections
    if (selectionType != 2) {
        showMessage("Error: incorrect tool", "Please select the 'Polygon' tool before proceeding.");
        return;
    }

    // Measure and draw polygon
    run("Measure");
    run("Draw");

    // Get polygon vertices
    getSelectionCoordinates(xPoints, yPoints);

    // Use last clicked point
    last = xPoints.length - 1;
    X = xPoints[last];
    Y = yPoints[last];

    // Draw PolygonID near the last clicked point
    drawLabel(X, Y, PolygonID);

    // Read measured area from Results
    row = nResults - 1;
    area = getResult("Area", row);

    // Save to custom table
    Table.set("Polygon-Nr", row, "" + PolygonID);
    Table.set("Area", row, area);
    Table.set("X", row, X);
    Table.set("Y", row, Y);
    Table.update;

    // Increment for next polygon
    PolygonID++;
}

// Macro entry
macro "Measure and Draw Polygon [1]" {
    measureAndDrawPolygon();
}