This is extracted from [an openZFS post](https://jro.io/truenas/openzfs/#arc) but it's very relevant to compiler design and puts things into perspective.

It's pretty difficult to wrap one's head around just how different latencies are for various parts of a computer system because we don't commonly deal in nanoseconds and milliseconds. To help understand just how slow hard drives really are, it's helpful to step through a thought experiment where we stretch time by a factor of 3 billion. In this expanded time scale, a 3GHz CPU would complete a single instruction every second. This lets us step into the perspective of the CPU which is waiting for data to be delivered by various types of storage. We can see these different types of storage listed below:

| **Storage Type**                            | **Slowed Time Scale** | **Real Time Scale** |
| ------------------------------------------- | --------------------- | ------------------- |
| Single CPU Instruction (at 3 GHz)           | 1 second              | 0.3 nSec            |
| Registers (storage for active instructions) | 1 to 3 seconds        | 0.3 to 1 nSec       |
| Level 1 Cache (on-CPU cache)                | 2 to 8 seconds        | 0.7 to 3 nSec       |
| Level 2 Cache (off-CPU but still on chip)   | 5 to 12 seconds       | 2 to 4 nSec         |
| **Main System Memory (RAM)**                | **30 to 60 seconds**  | **10 to 20 nSec**   |
| Intel Optane SSD                            | 6 to 15 hours         | 10 to 15 uSec       |
| NVMe SSD                                    | 3 to 11 days          | 100 to 200 uSec     |
| SAS/SATA SSD                                | 69 to 105 days        | 2 to 3 mSec         |
| 15K RPM HDD                                 | 105 to 210 days       | 3 to 6 mSec         |
| 10K RPM HDD                                 | 243 to 315 days       | 8 to 9 mSec         |
| **7.2K RPM HDD**                            | 315 to 525 days       | 10 to 15 mSec       |
| 5.4K RPM HDD                                | 525 to 700 days       | 15 to 20 mSec       |
| 3.5" Floppy Disk                            | 23.75 years!!         | 250 mSec            |