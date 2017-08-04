(
~readMarkersFromAIFF = { arg path;
	var f, len, magic, numMarkers, markPos, markName, markers, done, chunkLen;

	f = File(path, "rb");
	protect {
		magic = f.getInt32;
		if( magic != 0x464F524D, { Error( "File does not begin with FORM magic" ).throw });
		// trust the file length more than 32 bit form field which breaks
		// for > 2 GB (> 1 GB if using signed ints)
		f.getInt32;
		magic 	= f.getInt32;
		len		= f.length - 12;
		if ((magic != 0x41494646) && (magic != 0x41494643), {
			Error( "Format is not AIFF or AIFC" ).throw;
		});

		done     = false;
		chunkLen = 0;
		while({ done.not && (len > 0) }, {
			if (chunkLen != 0, { f.seek(chunkLen, 1)});   // skip to next chunk

			magic    = f.getInt32;
			chunkLen = (f.getInt32 + 1) & 0xFFFFFFFE;
			len      = len - (chunkLen + 8);
			switch(magic,
			0x4D41524B, {	// 'MARK'
				numMarkers	= f.getInt16;
				// getInt16 is signed, so for 32767 < numMarkers < 65536 we need to fix it
				if (numMarkers < 0, { numMarkers = numMarkers + 0x10000 });
				markers = Array.new(numMarkers);
				numMarkers.do({
					f.getInt16;             // marker ID (ignore)
					markPos	 = f.getInt32;  // sample frames
					markName = f.getPascalString;
					if (markName.size.even, { f.seek(1, 1)}); // next marker chunk on even offset
					markers.add(/* Event[ \name -> markName, \pos -> */ markPos /* ] */);
				});
				done = true;
			});
		});
	} { arg error;
		try { f.close };
	};
	markers.sort;
	markers;
}
)

(
// reads a chunk of a sound file.
// `start` and `stop` are frames into the file.
// the returned length will be `stop-start`.
// returns a multi-channel signal (array of arrays)
~readSoundChunk = { arg path, start, stop;
	var sf, len, sig;
	sf = SoundFile.openRead(path);
	start = start ? 0;
	stop = stop ? sf.numFrames;
	len = stop - start;
	sig = Signal.newClear(len * sf.numChannels);
	sf.seek(start, 0);
	sf.readData(sig);
	sf.close;
	sig.unlace(sf.numChannels);
};
)

~pathIn = "/data/temp/freesound_test3/320220_NACTA-6-SEG.aif";
~marks0 = ~readMarkersFromAIFF.(~pathIn);
~marks = [0] ++ ~marks0.collect { |x| x - 1024 } ++ [2949582];

~marks = [ 0, 8640, 21440, 27776, 40672, 52672, 62720, 68448, 76763, 81280, 90720, 96768, 104448, 110752, 120864, 131712, 139008, 151744, 161504, 173888, 179136, 192416, 200608, 206080, 212855, 219264, 230784, 240224, 246048, 253546, 259168, 266354, 272448, 285056, 295616, 308064, 312125, 320000, 325463, 333024, 338400, 345806, 351680, 359520, 365120, 371863, 377824, 384485, 391040, 398232, 404096, 411354, 417376, 424351, 429600, 437760, 443222, 451040, 457056, 463905, 470016, 477216, 484000, 493536, 498816, 512096, 520992, 527456, 534656, 539499, 547936, 553984, 560998, 566752, 580000, 592800, 602752, 608800, 615867, 622080, 630720, 636544, 644116, 649824, 658688, 663968, 671968, 678400, 691488, 704512, 713632, 720192, 733472, 740512, 747232, 753184, 760928, 771360, 783648, 790560, 798464, 804320, 810976, 817216, 824964, 830496, 843776, 857024, 866272, 872640, 879906, 885920, 893760, 899655, 906944, 919872, 926080, 939072, 952352, 961408, 966407, 973984, 980224, 987904, 993156, 1000544, 1006432, 1014030, 1019712, 1027488, 1033024, 1046304, 1056256, 1067168, 1074496, 1081254, 1086592, 1094784, 1100704, 1107690, 1113344, 1121472, 1126080, 1134126, 1139328, 1145952, 1151951, 1158688, 1164763, 1171968, 1177513, 1185120, 1190449, 1198400, 1203712, 1211011, 1216160, 1224000, 1229024, 1236134, 1242176, 1248946, 1255008, 1262656, 1268000, 1280992, 1292992, 1303744, 1314400, 1321216, 1328596, 1334496, 1341728, 1349056, 1358304, 1363744, 1376992, 1392091, 1397664, 1404216, 1412903, 1419104, 1426090, 1432192, 1445472, 1458720, 1474329, 1481504, 1488416, 1495394, 1501344, 1508146, 1514624, 1527904, 1541184, 1550080, 1555776, 1567944, 1577268, 1583547, 1590844, 1597347, 1610784, 1624064, 1631474, 1637152, 1644864, 1652096, 1661248, 1673715, 1678625, 1686432, 1692896, 1699712, 1705565, 1712896, 1718592, 1725943, 1731584, 1739194, 1744768, 1752071, 1758048, 1771104, 1785259, 1793491, 1799648, 1807156, 1812928, 1820720, 1826208, 1834560, 1839098, 1847744, 1854144, 1866183, 1878400, 1885472, 1892714, 1901856, 1915147, 1926752, 1933717, 1941056, 1947951, 1953952, 1962368, 1968576, 1981600, 1989568, 1996640, 2003808, 2009996, 2017088, 2023104, 2030874, 2035968, 2043488, 2050080, 2060416, 2070112, 2076992, 2090272, 2098146, 2103552, 2110898, 2116832, 2124087, 2130112, 2137440, 2143392, 2156672, 2169952, 2179616, 2184929, 2192640, 2197055, 2205728, 2210720, 2217548, 2223424, 2231552, 2236960, 2243926, 2248217, 2257120, 2262272, 2269059, 2274592, 2282368, 2287936, 2294874, 2300800, 2313440, 2325792, 2334208, 2339680, 2346147, 2351712, 2358898, 2364768, 2372206, 2378048, 2385083, 2390976, 2397487, 2403616, 2410848, 2416428, 2424128, 2430496, 2437618, 2443776, 2450848, 2457248, 2466560, 2471389, 2485440, 2497600, 2507392, 2512592, 2527328, 2540608, 2553504, 2561184, 2567456, 2574889, 2580640, 2587828, 2593920, 2607008, 2620224, 2633440, 2646400, 2656544, 2662432, 2675520, 2688256, 2697568, 2704512, 2717792, 2726656, 2732832, 2740448, 2747840, 2755570, 2762046, 2769984, 2777504, 2784896, 2792704, 2803392, 2815648, 2824032, 2837056, 2848640, 2855744, 2868192, 2881472, 2893568, 2906304, 2918528, 2926048, 2935616, 2949582 ];

(
// the 'mu_i' function in Gillet/Richard
~moment = { arg x, i;
	var len, num, denom, coef;
	len   = x.size;
	coef  = (0..(len-1)).pow(i);
	num   = (coef * x).sum;
	denom = x.sum;
	num/denom;
}
)

// takes amplitude spectrum as input, returns frequency in Hz
~centroid = { arg x; ~moment.value(x, i: 1) * ~sr / ~fftSize }

(
~corr = { arg x, y;
	var prod, n;
	n = x.size - 1;
	prod = (0..n).collect { arg i;
		x[i] * y[i]
	};
	prod.sum
}
)

// technically, a sine wave is an infinitely
// lasting signal, whereas the discrete auto-correlation
// can only be calculated over a finite window size.
// if we ensure that the signal array contains an
// integer number of periods, we can use "index" wrapping
// in the calculation of the delayed or lagged signal
// to simulate the infinite signal. e.g. when the
// time index becomes -1, we wrap it to the last element
// of the array, -2 becomes the penultimate element, etc.
(
~dly = { arg x, m;
	var len;
	len = x.size;
	(0..(len-1)).collect { arg i; x.wrapAt(i - m) };
};
)

// calculate the auto-correlation of a signal
// params: x - the signal array
//         m - the delay (or lag) in sample frames
(
~autoCorr = { arg x, m;
	var y;
	y = ~dly.value(x, m);
	~corr.value(x, y);
};
)

(
~pitch = { arg x, minFreqN, maxFreqN, thresh;
	var ac, minDly, maxDly, dly, threshAbs, isPeak, sizeM;
	ac = Array.fill(x.size, { arg m; ~autoCorr.value(x, m) });
	minDly = maxFreqN.reciprocal.floor.asInteger;
	maxDly = minFreqN.reciprocal.ceil .asInteger;
	threshAbs = ac[0] * thresh;
	sizeM = x.size - 1;
	// test if a local maximum above absolute thresh
	isPeak = { arg i;
		var isInside, isLocalMax;
		isInside   = (i > 0) && (i < sizeM);
		isLocalMax = isInside && { (ac[i-1] < ac[i]) && (ac[i+1] < ac[i]) };
		isLocalMax && (ac[i] > threshAbs);
	};
	dly = (0..sizeM).detect { arg i;
		(i >= minDly) && (i <= maxDly) && isPeak.value(i)
	};
	if (dly.notNil, { dly.reciprocal })
};
)

//////////

x = ~readSoundChunk.(~pathIn, ~marks[0], ~marks[1]);
y = x.sum;

// z = ~pitch.(y, minFreqN: 200/44100.0, maxFreqN: 800/44100.0, thresh: 0.3) *44100.0;

~fftSize = (y.size * 2 - 1).nextPowerOfTwo;
~cosTable = Signal.fftCosTable(~fftSize);
~zero     = Signal.newClear(~fftSize); // "imaginary part" of our real signals
~padded   = (y ++ Signal.fill(~fftSize - y.size, 0.0)).as(Signal);

~spec = fft(~padded, ~zero, ~cosTable);
~conj = ~spec.conjugate;
~prod = ~spec * ~conj;
~auto = ifft(~prod.real, ~prod.imag, ~cosTable).real.normalize;
~auto.plot.resolution_(0.25);

(
~pitch2 = { arg ac, minFreqN, maxFreqN, thresh;
	var minDly, maxDly, dly, threshAbs, isPeak, sizeM;
	minDly = maxFreqN.reciprocal.floor.asInteger;
	maxDly = minFreqN.reciprocal.ceil .asInteger;
	threshAbs = ac[0] * thresh;
	sizeM = ac.size - 1;
	// test if a local maximum above absolute thresh
	isPeak = { arg i;
		var isInside, isLocalMax;
		isInside   = (i > 0) && (i < sizeM);
		isLocalMax = isInside && { (ac[i-1] < ac[i]) && (ac[i+1] < ac[i]) };
		isLocalMax && (ac[i] > threshAbs);
	};
	dly = (0..sizeM).detect { arg i;
		(i >= minDly) && (i <= maxDly) && isPeak.value(i)
	};
	if (dly.notNil, { dly.reciprocal })
};
)

z = ~pitch2.(~auto, minFreqN: 200/44100.0, maxFreqN: 800/44100.0, thresh: 0.3) *44100.0;

~sr = 44100.0;

(
~pitch3 = { arg i;
	var sig, mono, fftSize, cosTable, zero, padded, start, stop, chunk, spec, conj, prod, auto, freq;
	start    = ~marks[i];
	stop     = ~marks[i+1];
	chunk    = stop - start;
	sig      = ~readSoundChunk.(~pathIn, start, stop);
	mono     = sig.sum;
	fftSize  = (chunk * 2 - 1).nextPowerOfTwo;
	cosTable = Signal.fftCosTable(fftSize);
	zero     = Signal.newClear(fftSize);
	padded   = (mono ++ Signal.fill(fftSize - chunk, 0.0)).as(Signal);
	spec     = fft(padded, zero, cosTable);
	conj     = spec.conjugate;
    prod     = spec * conj;
	auto     = ifft(prod.real, prod.imag, cosTable).real.normalize.keep(chunk);
	freq     = (~pitch2.(auto, minFreqN: 200/~sr, maxFreqN: 900/~sr, thresh: 0.3) ? 0) * ~sr;
	freq
}
)

~pitches = (0..(~marks.size-2)).collect { arg i; ~pitch3.(i) };
~pitches.plot(discrete: true);

~pitches = [ 474.1935483871, 450.0, 572.72727272727, 801.81818181818, 711.29032258065, 668.18181818182, 801.81818181818, 668.18181818182, 747.45762711864, 801.81818181818, 450.0, 668.18181818182, 648.52941176471, 537.80487804878, 537.80487804878, 648.52941176471, 711.29032258065, 711.29032258065, 711.29032258065, 711.29032258065, 900.0, 801.81818181818, 711.29032258065, 648.52941176471, 801.81818181818, 711.29032258065, 648.52941176471, 537.80487804878, 450.0, 572.72727272727, 648.52941176471, 711.29032258065, 537.80487804878, 537.80487804878, 537.80487804878, 580.26315789474, 450.0, 801.81818181818, 711.29032258065, 801.81818181818, 450.0, 572.72727272727, 648.52941176471, 537.80487804878, 450.0, 572.72727272727, 801.81818181818, 722.95081967213, 735.0, 580.26315789474, 450.0, 801.81818181818, 711.29032258065, 801.81818181818, 604.1095890411, 722.95081967213, 801.81818181818, 537.80487804878, 450.0, 648.52941176471, 711.29032258065, 801.81818181818, 711.29032258065, 711.29032258065, 711.29032258065, 658.20895522388, 787.5, 711.29032258065, 648.52941176471, 711.29032258065, 801.81818181818, 450.0, 572.72727272727, 450.0, 648.52941176471, 711.29032258065, 801.81818181818, 648.52941176471, 722.95081967213, 900.0, 801.81818181818, 450.0, 572.72727272727, 801.81818181818, 900.0, 801.81818181818, 711.29032258065, 735.0, 648.52941176471, 537.80487804878, 537.80487804878, 711.29032258065, 801.81818181818, 450.0, 531.32530120482, 531.32530120482, 537.80487804878, 537.80487804878, 801.81818181818, 668.18181818182, 639.13043478261, 801.81818181818, 450.0, 604.1095890411, 760.34482758621, 864.70588235294, 668.18181818182, 668.18181818182, 537.80487804878, 648.52941176471, 711.29032258065, 537.80487804878, 648.52941176471, 722.95081967213, 668.18181818182, 801.81818181818, 648.52941176471, 689.0625, 648.52941176471, 801.81818181818, 678.46153846154, 801.81818181818, 648.52941176471, 678.46153846154, 700.0, 537.80487804878, 450.0, 537.80487804878, 658.20895522388, 572.72727272727, 648.52941176471, 760.34482758621, 711.29032258065, 711.29032258065, 801.81818181818, 801.81818181818, 900.0, 801.81818181818, 450.0, 572.72727272727, 648.52941176471, 580.26315789474, 450.0, 537.80487804878, 801.81818181818, 495.50561797753, 479.34782608696, 479.34782608696, 801.81818181818, 711.29032258065, 648.52941176471, 801.81818181818, 722.95081967213, 801.81818181818, 450.0, 580.26315789474, 648.52941176471, 580.26315789474, 450.0, 572.72727272727, 801.81818181818, 700.0, 648.52941176471, 572.72727272727, 441.0, 711.29032258065, 537.80487804878, 537.80487804878, 580.26315789474, 450.0, 639.13043478261, 711.29032258065, 801.81818181818, 773.68421052632, 711.29032258065, 711.29032258065, 711.29032258065, 900.0, 900.0, 711.29032258065, 801.81818181818, 900.0, 801.81818181818, 450.0, 648.52941176471, 450.0, 648.52941176471, 760.34482758621, 801.81818181818, 711.29032258065, 450.0, 572.72727272727, 864.70588235294, 711.29032258065, 648.52941176471, 711.29032258065, 801.81818181818, 450.0, 450.0, 450.0, 580.26315789474, 801.81818181818, 711.29032258065, 648.52941176471, 801.81818181818, 668.18181818182, 604.1095890411, 537.80487804878, 537.80487804878, 639.13043478261, 711.29032258065, 639.13043478261, 801.81818181818, 722.95081967213, 648.52941176471, 580.26315789474, 450.0, 787.5, 711.29032258065, 801.81818181818, 450.0, 580.26315789474, 450.0, 648.52941176471, 760.34482758621, 711.29032258065, 711.29032258065, 801.81818181818, 450.0, 572.72727272727, 639.13043478261, 773.68421052632, 801.81818181818, 900.0, 711.29032258065, 450.0, 801.81818181818, 479.34782608696, 531.32530120482, 722.95081967213, 450.0, 801.81818181818, 450.0, 537.80487804878, 760.34482758621, 648.52941176471, 711.29032258065, 801.81818181818, 900.0, 900.0, 801.81818181818, 648.52941176471, 711.29032258065, 801.81818181818, 450.0, 572.72727272727, 801.81818181818, 787.5, 801.81818181818, 801.81818181818, 801.81818181818, 648.52941176471, 801.81818181818, 801.81818181818, 711.29032258065, 711.29032258065, 648.52941176471, 711.29032258065, 801.81818181818, 722.95081967213, 864.70588235294, 711.29032258065, 711.29032258065, 801.81818181818, 711.29032258065, 801.81818181818, 900.0, 801.81818181818, 450.0, 580.26315789474, 711.29032258065, 801.81818181818, 450.0, 572.72727272727, 801.81818181818, 711.29032258065, 450.0, 580.26315789474, 648.52941176471, 580.26315789474, 450.0, 787.5, 711.29032258065, 537.80487804878, 648.52941176471, 537.80487804878, 450.0, 537.80487804878, 801.81818181818, 722.95081967213, 689.0625, 580.26315789474, 450.0, 801.81818181818, 711.29032258065, 801.81818181818, 648.52941176471, 711.29032258065, 801.81818181818, 572.72727272727, 450.0, 648.52941176471, 711.29032258065, 801.81818181818, 711.29032258065, 711.29032258065, 711.29032258065, 900.0, 900.0, 801.81818181818, 900.0, 801.81818181818, 668.18181818182, 648.52941176471, 801.81818181818, 711.29032258065, 801.81818181818, 648.52941176471, 801.81818181818, 711.29032258065, 900.0, 801.81818181818, 711.29032258065, 735.0, 711.29032258065, 801.81818181818, 900.0, 648.52941176471, 787.5, 711.29032258065, 801.81818181818, 801.81818181818, 882.0, 801.81818181818, 450.0, 531.32530120482, 801.81818181818, 450.0, 668.18181818182, 639.13043478261, 537.80487804878, 537.80487804878, 648.52941176471, 760.34482758621, 801.81818181818, 801.81818181818, 537.80487804878, 537.80487804878, 787.5, 711.29032258065, 801.81818181818, 711.29032258065, 711.29032258065, 711.29032258065 ];

y = [ ~pitches, (0..(~pitches.size-1))].flop.sort { arg a, b; a[0] < b[0] }
~indicesPitch = y.collect(_.at(1))

~pathOut = "/data/temp/freesound_test3/320220_NACTA-6-byPitch.aif";
~sfOut = SoundFile.new.sampleFormat_("int16").numChannels_(2).sampleRate_(~sr);
~sfOut.openWrite(~pathOut);
~indicesPitch.do { arg i;
	x = ~readSoundChunk.(~pathIn, ~marks[i], ~marks[i+1]).lace.as(Signal);
	x.fade(beginSamp: 0, endSamp: 100, beginLevel: 0, endLevel: 1);
	x.fade(beginSamp: x.size - 100, endSamp: x.size, beginLevel: 1, endLevel: 0);
	~sfOut.writeData(x);
};
~sfOut.close;

///////////////

~sorted = ~indicesPitch.collect { arg i; [~marks[i], ~marks[i+1]] };

~lengths = ~sorted.collect { arg arr; arr[1] - arr[0] };
~sorted1 = [([0] ++ ~lengths).integrate.keep(~lengths.size-1), ~lengths.integrate].flop;
~sfOutM.writeData(~sorted1.flatten.as(Signal));
~sfOutM.close;

~pathOutM = "/data/temp/freesound_test3/320220_NACTA-6-byPitch-M.aif";
~sfOutM = SoundFile.new.sampleFormat_("float").numChannels_(2).sampleRate_(~sr);
~sfOutM.openWrite(~pathOutM);
~sfOutM.writeData(~sorted.flatten.as(Signal));
~sfOutM.close;

/////////

~bad = [11246, 15128, 90404, 380604, 1286241, 1338103, 1366459, 1383040, 1391948, 1395843, 1823190, 2014375, 2041541, 2049700, 2063195, 2068092, 2070891, 2117970];
~badI = ~bad.collect { arg m; ~sorted1.detectIndex { arg arr; arr[0] <= m and: { arr[1] > m }}};
~indicesPitchF = ~indicesPitch.reject { arg i, j; ~badI.includes(j) };

~pathOutF = "/data/temp/freesound_test3/320220_NACTA-6-byPitchF.aif";
~sfOutF = SoundFile.new.sampleFormat_("int16").numChannels_(2).sampleRate_(~sr);
~sfOutF.openWrite(~pathOutF);
~indicesPitchF.do { arg i;
	x = ~readSoundChunk.(~pathIn, ~marks[i], ~marks[i+1]).lace.as(Signal);
	x.fade(beginSamp: 0, endSamp: 100, beginLevel: 0, endLevel: 1);
	x.fade(beginSamp: x.size - 100, endSamp: x.size, beginLevel: 1, endLevel: 0);
	~sfOutF.writeData(x);
};
~sfOutF.close;

/////////////

~sortedF = ~indicesPitchF.collect { arg i; [~marks[i], ~marks[i+1]] };
~lengthsF = ~sortedF.collect { arg arr; arr[1] - arr[0] };
~sortedF1 = [([0] ++ ~lengthsF).integrate.keep(~lengthsF.size), ~lengthsF.integrate].flop;
// ~sortedF1.size;
~indicesPitchF.collect { arg i; ~pitches[i] } .plot(name: "Pitches", discrete: true);

~pathOutFM = "/data/temp/freesound_test3/320220_NACTA-6-byPitchF-M.aif";
~sfOutFM = SoundFile.new.sampleFormat_("float").numChannels_(2).sampleRate_(~sr);
~sfOutFM.openWrite(~pathOutFM);
~sfOutFM.writeData(~sortedF1.flatten.as(Signal));
~sfOutFM.close;

/////////////
([0] ++ ~lengthsF).integrate.asCompileString

s.boot;
~bufSoundF = Buffer.read(s, ~pathOutF);
~bufMarksF = Buffer.sendCollection(s, ~sortedF1.flatten, numChannels: 2);

~sortedF1.size
(
x = play {
	var value, trig, startPos, state, numRegions, regionStart, regionStop, regionDur;
	var voiceEnv, voiceEG, voiceEGOn, voiceOnOff, stateOutKr, stateInKr, activated;

	stateInKr = LocalIn.kr(1); // numChannels: 1, default: 0);

	numRegions = BufFrames.ir(~bufMarksF.bufnum);
	value = MouseY.kr(0, numRegions - 1, lag: 0).round;
	regionStart = Index.kr(~bufMarksF.bufnum, value * 2 + 0);
	regionStop  = Index.kr(~bufMarksF.bufnum, value * 2 + 1);

	activated  = stateInKr <= 0;

	regionDur  = Gate.kr((regionStop - regionStart) / BufSampleRate.ir(~bufMarksF.bufnum), activated);
	voiceEnv   = Env.perc(attackTime: 0, releaseTime: regionDur, curve: \lin);
	voiceEG    = EnvGen.ar(voiceEnv, gate: activated);
	voiceEGOn  = A2K.kr(voiceEG) > 0;
	voiceOnOff = activated | voiceEGOn;

	stateOutKr  = voiceOnOff;

	LocalOut.kr(stateOutKr);

	[regionStart, regionStop].poll(activated);

	0;
	// Index
	// BufRd
	//
	// PlayBuf.ar(numChannels: 2, bufnum: ~bufSoundF, rate: BufRateScale.ir(~bufSoundF),
	// trigger: trig, startPos: startPos);
}
)
