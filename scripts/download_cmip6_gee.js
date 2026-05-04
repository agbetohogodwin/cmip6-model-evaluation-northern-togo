var zone_etude = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point([0.201023, 10.873306]), {Localite: 'Dapaong'}),
  ee.Feature(ee.Geometry.Point([0.4703, 10.3569]),    {Localite: 'Mango'}),
  ee.Feature(ee.Geometry.Point([1.1861, 9.5511]),     {Localite: 'Kara'}),
  ee.Feature(ee.Geometry.Point([1.1017, 9.7664]),     {Localite: 'Niamtougou'}),
]);

var models = [
  'UKESM1-0-LL', 'INM-CM4-8', 'FGOALS-g3', 'KACE-1-0-G',
  'MIROC-ES2L', 'HadGEM3-GC31-MM', 'INM-CM5-0', 'CanESM5',
  'HadGEM3-GC31-LL', 'BCC-CSM2-MR', 'ACCESS-ESM1-5', 'GISS-E2-1-G',
  'ACCESS-CM2', 'GFDL-ESM4', 'MIROC6', 'EC-Earth3',
  'IPSL-CM6A-LR', 'MRI-ESM2-0', 'CNRM-ESM2-1', 'CNRM-CM6-1',
  'EC-Earth3-Veg-LR', 'MPI-ESM1-2-LR', 'NorESM2-LM', 'GFDL-CM4',
  'NorESM2-MM', 'MPI-ESM1-2-HR'
];

models.forEach(function(model) {
  var historical = ee.ImageCollection('NASA/GDDP-CMIP6')
    .filter(ee.Filter.eq('model', model))
    .filter(ee.Filter.eq('scenario', 'historical'))
    .filterDate('1983-01-01', '2014-12-31')
    .select(['pr', 'tasmax', 'tasmin']);

  var extract_hist = historical.map(function(img) {
    var stats = img.reduceRegions({
      collection: zone_etude,
      reducer: ee.Reducer.mean(),
      scale: 25000
    });
    return stats.map(function(f) {
      return f.set('date', img.date().format('YYYY-MM-dd'))
              .set('model', model)
              .set('scenario', 'historical');
    });
  }).flatten();

  Export.table.toDrive({
    collection: extract_hist,
    description: model + '_historical',
    folder: 'CMIP6_evaluation',
    fileNamePrefix: model + '_historical',
    fileFormat: 'CSV',
    selectors: ['model', 'scenario', 'Localite', 'date', 'pr', 'tasmax', 'tasmin']
  });
});

Map.centerObject(zone_etude, 7);
Map.addLayer(zone_etude, {color: 'red'}, 'Dapaong, Mango, Kara, Niamtougou');