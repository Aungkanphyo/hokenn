import csv
import shutil
import time

def process_batch():
    declareFile = 'Data/screen4_output.csv'
    appFile = 'Data/T_Application.csv'
    appFileTmp = 'Data/T_Application_Batch_Tmp.csv'
    declareTmp = 'Data/scree4_tmp.csv'

    pendingUpdate = {}

    try:
        with open(declareFile, 'r', encoding='utf-8') as f_in, \
            open(declareTmp, 'w', encoding='utf-8', newline='') as f_out:
            
            reader = csv.reader(f_in)
            writer = csv.writer(f_out)

            for row in reader:
                if len(row) >= 3:
                    imei = row[0].strip()

                    statusColIndex = None
                    currentStatus = ""
                    totalScore = 0

                    for index in range(2, len(row)):
                        colData = row[index]

                        if ':' in colData:
                            # ':' based on the question name (Key) and score (Value) are separated
                            key, value = colData.split(':', 1)
                            key = key.strip().upper()
                            value = value.strip()

                            # If it contains the word 'STATUS', it will be marked as a Status Column
                            if 'STATUS' in key:
                                statusColIndex = index
                                currentStatus = value.upper()
                            else:
                                # will add up all the numbers in the remaining columns except Status
                                try:
                                    totalScore += int(value)
                                except ValueError:
                                    pass

                    if statusColIndex is not None:
                        if currentStatus == 'REJECTED':
                            pendingUpdate[imei] = 'Rejected'
                            row[statusColIndex] = row[statusColIndex].replace('REJECTED', 'COMPLETED')
                        
                        elif currentStatus == 'APPROVED':
                            pendingUpdate[imei] = 'Accepted'
                            row[statusColIndex] = row[statusColIndex].replace('APPROVED', 'COMPLETED')

                        elif currentStatus == 'PENDING':
                            if totalScore < 70:
                                pendingUpdate[imei] = 'Accepted'
                                row[statusColIndex] = row[statusColIndex].replace('PENDING', 'APPROVED')
                            else:
                                pendingUpdate[imei] = 'Rejected'
                                row[statusColIndex] = row[statusColIndex].replace('PENDING', 'REJECTED')

                writer.writerow(row)

        # Replacing the original screen4_output.csv with the updated data
        shutil.move(declareTmp, declareFile)
    
    except FileNotFoundError:
        print(f"Error: {declareFile} file not found.")
        return
    
    # If there is no Pending Status to update, stop T_Application and continue.
    if not pendingUpdate:
        print(f"[{time.strftime('%X')}] No Pending status to update. (Checking again in 30s...)")
        return
    
    # Updating T_Application.csv using T_Application_Batch_Tmp.csv
    try:
        with open(appFile, 'r', encoding='utf-8') as f_in, \
            open(appFileTmp, 'w', encoding='utf-8', newline='') as f_out:
            
            reader = csv.reader(f_in)
            writer = csv.writer(f_out)

            header = next(reader)
            writer.writerow(header)

            # Searching for header column indexes
            cleanHeader = [h.strip().upper() for h in header]
            try:
                imeiIndex = cleanHeader.index('IMEI')
                statusIndex = cleanHeader.index('STATUS')
            except ValueError:
                # If the column name is incorrect, set the default index
                imeiIndex = 16
                statusIndex = 8

            updatedCount = 0
            for row in reader:
                if len(row) > imeiIndex and len(row) > statusIndex:
                    currentImei = row[imeiIndex].strip()
                    status = row[statusIndex].strip().upper()

                    if currentImei in pendingUpdate and status == 'PENDING':
                        row[statusIndex] = pendingUpdate[currentImei]
                        updatedCount += 1

                writer.writerow(row)

        shutil.move(appFileTmp, appFile)
        print(f"[{time.strftime('%X')}] success. (New Status Change in T_Application: {updatedCount})")        

    except FileNotFoundError:
        print(f"Error: {appFile} file not found.")    


# To run a batch in a loop every 30 seconds
if __name__=="__main__":
    print("Batch program has started running...")
    while True:
        process_batch()
        time.sleep(30)