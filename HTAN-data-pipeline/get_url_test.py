from MetadataModel import MetadataModel

#inputMModelLocation = "./schemas/exampleSchemaReq.jsonld"
# inputMModelLocation = "/Users/xdoan/Shell/HTAN-data-pipeline/schemas/scRNASeq.jsonld"
inputMModelLocation = "/Users/xdoan/Shell/HTAN-data-pipeline/schemas/scRNASeq.jsonld"
inputMModelLocationType = "local"
datasetType = "scRNASeq"

mm = MetadataModel(inputMModelLocation, inputMModelLocationType)

manifest_url = mm.getModelManifest(datasetType, additionalMetadata = {"Filename":["MantonCB1_HiSeq_1_S1_L001_R1_001.fastq.gz", "MantonCB1_HiSeq_1_S1_L001_R2_001.fastq.gz", "MantonCB1_HiSeq_1_S1_L002_R1_001.fastq.gz", "MantonCB1_HiSeq_1_S1_L002_R1_001.fastq.gz"]})

print(manifest_url)

