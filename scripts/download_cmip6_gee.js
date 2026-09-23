// 1. ZONE D'ÉTUDE (4 localités)
var zone_etude = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point([0.201023, 10.873306]), {Localite: 'Dapaong'}),
  ee.Feature(ee.Geometry.Point([0.4703, 10.3569]),    {Localite: 'Mango'}),
  ee.Feature(ee.Geometry.Point([1.1861, 9.5511]),     {Localite: 'Kara'}),
  ee.Feature(ee.Geometry.Point([1.1017, 9.7664]),     {Localite: 'Niamtougou'}),
]);

// 2. CONFIGURATION DES MODÈLES
// Les modèles standards n'ont pas besoin de grille.
// GFDL-CM4 a deux grilles distinctes (gr1 et gr2), traitées séparément.
var modelNames = [
  'ACCESS-CM2', 'ACCESS-ESM1-5', 'BCC-CSM2-MR', 'CanESM5',
  'CMCC-CM2-SR5', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-ESM2-1',
  'EC-Earth3', 'EC-Earth3-Veg-LR', 'FGOALS-g3', 'GFDL-ESM4',
  'GISS-E2-1-G', 'HadGEM3-GC31-LL', 'HadGEM3-GC31-MM', 'INM-CM4-8',
  'INM-CM5-0', 'IPSL-CM6A-LR', 'KACE-1-0-G', 'KIOST-ESM',
  'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR',
  'MRI-ESM2-0', 'NESM3', 'NorESM2-LM', 'NorESM2-MM',
  'TaiESM1', 'UKESM1-0-LL'
];

var modelConfig = modelNames.map(function(name) {
  return {name: name, grid: null, outputName: name};
});

modelConfig.push({name: 'GFDL-CM4', grid: 'gr1', outputName: 'GFDL-CM4_gr1'});
modelConfig.push({name: 'GFDL-CM4', grid: 'gr2', outputName: 'GFDL-CM4_gr2'});

// 3. FONCTION DE RÉDUCTION SPATIALE
var extractStats = function(imgCollection, outputName, scenarioName, zone) {
  return imgCollection.map(function(img) {
    var stats = img.reduceRegions({
      collection: zone,
      reducer: ee.Reducer.mean(),
      scale: 25000
    });
    return stats.map(function(f) {
      return f.set('date', img.date().format('YYYY-MM-dd'))
              .set('model', outputName)
              .set('scenario', scenarioName);
    });
  }).flatten();
};

// 4. BOUCLE SUR LES 32 MODÈLES
modelConfig.forEach(function(cfg) {

  var baseCollection = ee.ImageCollection('NASA/GDDP-CMIP6')
    .filter(ee.Filter.eq('model', cfg.name));

  if (cfg.grid !== null) {
    baseCollection = baseCollection.filter(ee.Filter.eq('grid_label', cfg.grid));
  }

  var histColl = baseCollection
    .filter(ee.Filter.eq('scenario', 'historical'))
    .filterDate('1983-01-01', '2014-12-31')
    .select(['pr', 'tasmax', 'tasmin']);

  var extractedHist = extractStats(histColl, cfg.outputName, 'historical', zone_etude);

  Export.table.toDrive({
    collection: extractedHist,
    description: cfg.outputName + '_historical',
    folder: 'CMIP6_evaluation',
    fileNamePrefix: cfg.outputName + '_historical',
    fileFormat: 'CSV',
    selectors: ['model', 'scenario', 'Localite', 'date', 'pr', 'tasmax', 'tasmin']
  });
});

Map.centerObject(zone_etude, 7);
Map.addLayer(zone_etude, {color: 'red'}, 'Dapaong, Mango, Kara, Niamtougou');

print('32 tâches d\'exportation (historical, pr/tasmax/tasmin) générées.');
