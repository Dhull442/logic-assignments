% Adjacent function
adj(A,B,[A | [B | _]]).
adj(A,B,[B | [A | _]]).
adj(A,B,[_ | T]) :- adj(A,B,T).

% In the order given
adj1(A,B,[A|[B|_]]).
adj1(A,B,[_|T]):- adj1(A,B,T).

% In reverse of the order given
adj2(A,B,[B|[A|_]]).
adj2(A,B,[_|T]):- adj2(A,B,T).

matches(X,(L,S)):- S = X , lines(Lines), member(L, Lines), line(L, Ls), member(S,Ls).

% Converting Line color to Subsequent list.
line(C,X):- C == yellow, yellowLine(X).
line(C,X):- C == green, greenLine(X).
line(C,X):- C == greenbranch, greenbranchLine(X).
line(C,X):- C == magenta, magentaLine(X).
line(C,X):- C == pink, pinkLine(X).
line(C,X):- C == orange, orangeLine(X).
line(C,X):- C == blue, blueLine(X).
line(C,X):- C == blue, bluebranchLine(X).
line(C,X):- C == grey, greyLine(X).
line(C,X):- C == red, redLine(X).
line(C,X):- C == violet, violetLine(X).

% Definition of Hop
hop((LX,SX),(LY,SY)):- hop0((LX,SX),(LY,SY)).
hop((LX,SX),(LY,SY)):- hop1((LX,SX),(LY,SY)).
hop((LX,SX),(LY,SY)):- hop2((LX,SX),(LY,SY)).

% 3 types of hop: 2 on the same line in different directions, 1 on a different line.
hop1((LX,SX),(LY,SY)):- LY = LX, line(LX,X), adj1(SX,SY,X).
hop2((LX,SX),(LY,SY)):- LY = LX, line(LX,X), adj2(SX,SY,X).
hop0((LX,SX),(LY,SY)):- SY = SX, lines(L), member(LY,L), not(LX==LY), line(LY,X), member(SX,X).

list_pair(Ls, L-Ls):- length(Ls,L).
sorttop(L,LL):- maplist(list_pair, L, Pairs0), keysort(Pairs0,LL).
top5([_-L0,_-L1,_-L2,_-L3,_-L4 | _],L):- L = [L0,L1,L2,L3,L4].
paths(X,Y,LL):- findall(LL1,pathfromstations(X,Y,LL1),Lall), restruct(Lall,L), sorttop(L,L1),top5(L1,LL).
pathfromstations(X,Y,LL):- matches(X,Xs), matches(Y,Ys), path(Xs,Ys,0,LL).

% Restructure list of lists into list.
restruct([L1|T],L):- append(L2,L1,L), restruct(T,L2).
restruct([],[]).

% Increment K till we get shortest 5 paths.
path(X,Y,K,LL):- K< 50, findall(LL1,pathofdepth(X,Y,3,K,LL1),L), length(L,Len), Len < 5, K1 is K + 1,  path(X,Y,K1,LL).
path(X,Y,K,LL):- K< 50, findall(LL1,pathofdepth(X,Y,3,K,LL1),L), length(L,Len), Len > 4, LL = L.

% Depth bound path search
pathofdepth(X,Y,_,K,LL):- hop(X,Y), K > 0, LL = [X,Y].

pathofdepth(X,Y,T,K,LL):- T==3, hop0(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,0,K1,LL1), not(member(X,LL1)), LL = [X|LL1].
pathofdepth(X,Y,T,K,LL):- T==3, hop1(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,1,K1,LL1), not(member(X,LL1)), LL = [X|LL1].
pathofdepth(X,Y,T,K,LL):- T==3, hop2(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,2,K1,LL1), not(member(X,LL1)), LL = [X|LL1].

pathofdepth(X,Y,T,K,LL):- T==0, hop1(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,1,K1,LL1), not(member(X,LL1)), LL = [X|LL1].
pathofdepth(X,Y,T,K,LL):- T==0, hop2(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,2,K1,LL1), not(member(X,LL1)), LL = [X|LL1].

pathofdepth(X,Y,T,K,LL):- T==1, hop1(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,1,K1,LL1), not(member(X,LL1)), LL = [X|LL1].
pathofdepth(X,Y,T,K,LL):- T==1, hop0(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,0,K1,LL1), not(member(X,LL1)), LL = [X|LL1].

pathofdepth(X,Y,T,K,LL):- T==2, hop2(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,2,K1,LL1), not(member(X,LL1)), LL = [X|LL1].
pathofdepth(X,Y,T,K,LL):- T==2, hop0(X,M), K > 0, K1 is K-1, pathofdepth(M,Y,0,K1,LL1), not(member(X,LL1)), LL = [X|LL1].

printlist([]).
printlist([X|List]):- write(X), write(","), printlist(List).

printlists([L1| T]):- printlist(L1), write('\n'), printlists(T).
printlists([]).

% Color Lines
lines([yellow,
green,
greenbranch,
magenta,
pink,
orange,
blue,
bluebranch,
grey,
red,
violet]).


% database of metro
yellowLine([samaypurBadli,
	rohiniSector18,
	haiderpur,
	jahangirpuri,
	adarshNagar,
	azadpur,
	modelTown,
	guruTeghBahadurNagar,
	vishwaVidyalaya,
	vidhanSabha,
	civilLines,
	kashmereGate,
	chandniChowk,
	chawriBazar,
	newDelhi,
	rajivChowk,
	patelChowk,
	centralSecretariat,
	udyogBhawan,
	lokKalyanMarg,
	jorbagh,
	dilliHaatINA,
	aiims,
	greenPark,
	hauzKhas,
	malviyaNagar,
	saket,
	qutabMinar,
	chhatarpur,
	sultanpur,
	ghitorni,
	arjanGarh,
	guruDronacharya,
	sikandarpur,
	mgRoad,
	iffcoChowk,
	hudaCityCentre]).
greenLine([inderlok,
	ashokParkMain,
	punjabiBagh,
	shivajiPark,
	madipur,
	paschimViharEast,
	paschimViharWest,
	peeraGarhi,
	udyogNagar,
	maharajaSurajmalStadium,
	nangloi,
	nangloiRailwayStation,
	rajdhaniPark,
	mundka,
	mundkaIndustrialArea,
	ghevra,
	tikriKalan,
	tikriBorder,
	panditShreeRamSharma,
	bahadurgarhCity,
	brigadierHoshiyarSingh]).
greenbranchLine([ashokParkMain,
	satguruRamsinghMarg,
	kirtiNagar]).
magentaLine([janakPuriWest,
	dabriMor,
	dashrathPuri,
	palam,
	sadarBazaarCantonment,
	terminal1IGIAirport,
	shankarVihar,
	vasantVihar,
	munirka,
	rkPuram,
	iitDelhi,
	hauzKhas,
	panchsheelPark,
	chiragDelhi,
	greaterKailash,
	nehruEnclave,
	kalkajiMandir,
	okhlaNSIC,
	sukhdevVihar,
	jamiaMilliaIslamia,
	okhlaVihar,
	jasolaViharShaheenBagh,
	kalindiKunj,
	okhlaBirdSanctuary,
	botanicalGarden]).
pinkLine([majlisPark,
	azadpur,
	shalimarBagh,
	netajiSubhashPlace,
	shakurpur,
	punjabiBaghWest,
	esiBasaiDarapur,
	rajouriGarden,
	mayaPuri,
	narainaVihar,
	delhiCantt,
	durgabaiDeshmukhSouthCampus,
	sirVishweshwaraiahMotiBagh,
	bhikajiCamaPlace,
	dilliHaatINA,
	southExtension,
	lajpatNagar,
	vinobapuri,
	ashram,
	saraiKaleKhanHazratNizamuddin,
	mayurVihar1,
	mayurViharPocket1,
	trilokpuriSanjayLake,
	vinodNagarEast,
	mandawali,
	ipExtension,
	anandVihar,
	karkarduma,
	karkardumaCourt,
	krishnaNagar,
	eastAzadNagar,
	welcome,
	jaffrabad,
	maujpur,
	gokulpuri,
	johriEnclave,
	shivVihar]).
orangeLine([newDelhi,
	shivajiStadium,
	dhaulaKuan,
	delhiAerocity,
	igiAirport,
	dwarkaSector21]).
bluebranchLine([yamunaBank,
	laxmiNagar,
	nirmanVihar,
	preetVihar,
	karkarduma,
	anandVihar,
	kaushambi,
	vaishali]).
greyLine([dwarka,
      nangli,
      najafgarh]).
blueLine([dwarkaSector21,
      dwarkaSector8,
      dwarkaSector9,
      dwarkaSector10,
      dwarkaSector11,
      dwarkaSector12,
      dwarkaSector13,
      dwarkaSector14,
      dwarka,
      dwarkaMor,
      nawada,
      uttamNagarWest,
      uttamNagarEast,
      janakPuriWest,
      janakPuriEast,
      tilakNagar,
      subhashNagar,
      tagoreGarden,
      rajouriGarden,
      rameshNagar,
      motiNagar,
      kirtiNagar,
      shadipur,
      patelNagar,
      rajendraPlace,
      karolBagh,
      jhandewalan,
      rkAshramMarg,
      rajivChowk,
      barakhamba,
      mandiHouse,
      supremeCourt,
      indraprastha,
      yamunaBank,
      akshardham,
      mayurVihar1,
      mayurViharExtention,
      newAshokNagar,
      noidaSector15,
      noidaSector16,
      noidaSector18,
      botanicalGarden,
      golfCourse,
      noidaCityCenter,
      noidaSector34,
      noidaSector52,
      noidaSector61,
      noidaSector59,
      noidaSector62,
      noidaElectronicCity]).
redLine([shaheedSthal,
     hindonRiver,
     arthala,
     mohanNagar,
     shyamPark,
     majorMohitSharma,
     rajBagh,
     shaheedNagar,
     dilshadGarden,
     jhilMil,
     mansaroverPark,
     shahdara,
     welcome,
     seelampur,
     shastriPark,
     kashmereGate,
     tisHazari,
     pulBangash,
     pratapNagar,
     shastriNagar,
     inderlok,
     kanhaiyaNagar,
     keshavPuram,
     netajiSubhashPlace,
     kohatEnclave,
     pitamPura,
     rohiniEast,
     rohiniWest,
     rithala]).
violetLine([kashmereGate,
	lalQuila,
	jamaMasjid,
	delhiGate,
	ito,
	mandiHouse,
	janpath,
	centralSecretariat,
	khanMarket,
	jawaharlalNehruStadium,
	jangpura,
	lajpatNagar,
	moolchand,
	kailashColony,
	nehruPlace,
	kalkajiMandir,
	govindPuri,
	okhla,
	jasola,
	saritaVihar,
	mohanEstate,
	tughlakabad,
	badarpurBorder,
	sarai,
	nhpcChowk,
	mewalaMaharajpur,
	sector28Faridabad,
	badkalMor,
	oldFaridabad,
	neelamChowkAjronda,
	bataChowk,
	escortsMujesar,
	santSurdasSihi,
	rajaNaharSingh]).
% Database ends
