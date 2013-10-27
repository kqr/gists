//
// Copyright (c) 2012, kqr
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * The names of the authors must not be used to endorse or promote
//       products derived from this software without specific prior written
//       permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//


// A simple processing sketch to draw the ear reddening move in the style of
// http://i.solidfiles.net/171026c761.jpg


// The resolution of the thing
int sizeX = 1280;
int sizeY = 800;

// The size of each square of the board
// Play around to find something that suits you
int gridSize = min(sizeX, sizeY)/36;

// Center the board
int boardOffsetX = floor(sizeX/2 - gridSize*9.5);
int boardOffsetY = floor(sizeY/2 - gridSize*9.5);

// O represents white stone
// # represents black stone
String[] stones = {
  "         #OO       ",
  "   #     #O O OO#  ",
  "  OO #  O##OO O#   ",
  "           ###  #  ",
  "     #    #    ##  ",
  "  O            #OO ",
  "             OOO###",
  "              #OOO#",
  "         #  #OO### ",
  "            OO# #O ",
  "  O           O##O ",
  "              O#O# ",
  "            O O#OO ",
  "  O      # #O O#   ",
  "      # O  #O#O#O  ",
  "  # #  #   #OO#OO  ",
  "     #O#O OO####OO ",
  "      #O OO O## #O ",
  "        O  O # # # "
};

// This colour dictates the general colour scheme of the board
color boardColour = color(130, 105, 4);

// These are the colours for the stones. They are automatically
// configured to match the current board colour unless you set
// them manually here or anywhere else.
color whiteColour = color(0,0,0);
color blackColour = color(0,0,0);

void setup() {
  size(sizeX,sizeY);
  noLoop();
  
  colorMode(HSB, 100, 100, 100);
  if (whiteColour == blackColour) {
    // Set stone colour to something that makes sense with
    // the current board colour
    whiteColour = color(hue(boardColour), saturation(boardColour) / 2, brightness(boardColour) * 3);
    blackColour = color(hue(boardColour), saturation(boardColour), brightness(boardColour) / 2);
  }
  colorMode(RGB, 255, 255, 255);
}

void draw() {
  // Draw the board
  background(boardColour);
  
  for (int i = 0; i < 19; i++) {
    stroke(whiteColour, random(12, 32));
    line(boardOffsetX+gridSize*i, 0, boardOffsetX+gridSize*i, sizeY);
  }
  for (int i = 0; i < 19; i++) {
    stroke(whiteColour, random(12, 32));
    line(0, boardOffsetY+gridSize*i, sizeX, boardOffsetY+gridSize*i);
  }
  
  // Draw all the stones
  for (int j = 0; j < 19; j++) {
    for (int i = 0; i < 19; i++) {
      stone(i, j, stones[j].charAt(i));
    }
  }
  
  // Draw the highlighting on the ear reddening move
  // Drawing a cross is complicated :(
  hint(ENABLE_STROKE_PURE);
  stroke(255,248,170);
  line(boardOffsetX+9*gridSize - gridSize/5,
       boardOffsetY+8*gridSize - gridSize/5,
       boardOffsetX+9*gridSize + gridSize/5,
       boardOffsetY+8*gridSize + gridSize/5);
  line(boardOffsetX+9*gridSize - gridSize/5,
       boardOffsetY+8*gridSize + gridSize/5,
       boardOffsetX+9*gridSize + gridSize/5,
       boardOffsetY+8*gridSize - gridSize/5);
  
  // Apply grain noise
  grain();
  
  // Apply vignetting
  vignetting();
  
  // Save image!
  save("earreddening_" + sizeX + "x" + sizeY + ".jpg");
}



// This method draws a stone in a coordinate on the board
// and with a colour according to the player number
void stone(int x, int y, char player) {
  switch (player) {
    case 'O': fill(whiteColour); break;
    case '#': fill(blackColour); break;
    default: noFill();
  }
  
  noStroke();
  ellipseMode(CENTER);
  ellipse(boardOffsetX+x*gridSize,boardOffsetY+y*gridSize,gridSize,gridSize);
}


// This method applies the vignetting by linearly interpolating
// between current colour and black, depending on how "far out"
// on the board the pixel is.
void vignetting() {
  color vignetteColour = color(0,0,0);
  // At maxDist pixels from the centre of the board, the colour
  // should be black. Change this for a more or less extreme
  // vignetting effect.
  float maxDist = dist(0, 0, sizeX/2, sizeY/2)*1.0;
  
  // Calculate and apply the correct amount of vignetting for
  // each pixel.
  loadPixels();
  for (int j = 0; j < sizeY; j++) {
    for (int i = 0; i < sizeX; i++) {
      float distance = dist(i, j, sizeX/2, sizeY/2);
      pixels[j*sizeX + i] = lerpColor(pixels[j*sizeX + i], vignetteColour, sq(distance/maxDist));
    }
  }
  updatePixels();
}


// This method applies the noise, by randomly interpolating
// between the current colour and the noise colours for every
// pixel.
void grain() {
  float noiseStrength = 0.025;
  loadPixels();
  for (int i = 0; i < sizeX*sizeY; i++) {
    color noiseColour = pixels[i];
    
    // First noise pass -- chromatic noise
    noiseColour = lerpColor(noiseColour, color(random(0, 255), random(0, 255), random(0, 255)), noiseStrength * random(1) * 2);
    // Second noise pass -- grey noise
    noiseColour = lerpColor(noiseColour, color(random(0, 255)), noiseStrength * random(1) * 4);
    // Third noise pass -- background coloured noise
    noiseColour = lerpColor(noiseColour, boardColour, noiseStrength * random(1));
    
    pixels[i] = noiseColour;
  }
  updatePixels();
}
