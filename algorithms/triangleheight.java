
public float getHeight(float x, float y) {

	/*
	 * The coordinates of the first vertex in the quad within which
	 * you want to know a height.
	 */

	int v1x = (int) (x);
	int v1y = (int) (y);
	float v1z = groundTiles[v1x][v1y].height;


	/*
	 * The second vertex is diagonally opposed.
	 */

	int v2x = v1x + 1;
	int v2y = v1y + 1;
	float v2z = groundTiles[v2x][v2y].height;


	/*
	 * Coordinates for the normal vector.
	 */

	float nx, ny, nz;
		
	/*
	 * The following decides which triangle on the quad the height is requested for.
	 * The coordinates for the third vertex is either on the x axis or the y axis.
	 * Depending on which triangle, it performs the calculations for the normal
	 * vector differently.
	 *
	 * The normal vector calculations are just simplifications of
	 *
	 *     (v2 - v1) × (v3 - v1)
	 *
	 * or
	 *
	 *     (v3 - v1) × (v2 - v1)
	 *
	 * respectively.
	 */
		
	if (GameUtils.point2Distance(x, y, v1x, v1y + 1) < GameUtils.point2Distance(x, y, v1x + 1, v1y)) {
		int v3x = (int) v1x;
		int v3y = (int) v1y + 1;
		float v3z = groundTiles[v3x][v3y].height;

		nx = v3z - v2z;
		ny = v1z - v3z;
		nz = 1;
	} else {
		int v3x = (int) v1x + 1;
		int v3y = (int) v1y;
		float v3z = groundTiles[v3x][v3y].height;
			
		nx = v1z - v3z;
		ny = v3z - v2z;
		nz = 1;
	}
		
		
	/*
	 * Magic math for figuring out the height of a point on a triangle
	 * based on the normal, another point on the triangle and two coordinates
	 * of the point in question.
	 */
		
	return (nx * (x - v1x) + ny * (y - v1y)) / (-nz) + v1z;
		
}