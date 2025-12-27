import {CommonModule} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';
import {PageResponse} from '../../../../responses/page-response';
import {ResponseObject} from '../../../../responses/response-object';
import {LichSuTranDauResponse} from '../../../../responses/trandau/lichsutrandau';
import {TranDauResponse} from '../../../../responses/trandau/trandau-response';
import {Base} from '../../../base/base';
import {NgbDropdownModule} from '@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'app-admin-trandau-list',
  standalone: true,
  imports: [CommonModule, FormsModule, NgbDropdownModule],
  templateUrl: './admin-trandau-list.html',
  styleUrl: './admin-trandau-list.scss',
})
export class AdminTranDauList extends Base implements OnInit {
  // ===================== THỐNG KÊ =====================
  stats = {
    totalBattles: 0,
    pendingBattles: 0,
    ongoingBattles: 0,
    finishedBattles: 0,
    todayBattles: 0,
    totalHistories: 0,
  };
  loadingStats = false;

  // ===================== PHÒNG ĐANG CHỜ =====================
  loadingPending = false;
  pendingBattles: TranDauResponse[] = [];

  // Modal chi tiết phòng
  showRoomModal = false;
  roomModalLoading = false;
  roomDetail: any = null;

  // ===================== LỊCH SỬ TRẬN ĐẤU =====================
  loadingHistory = false;
  historyItems: LichSuTranDauResponse[] = [];
  historyPage = 0;
  historyLimit = 10;
  historyTotalPages = 0;

  // Filter nâng cao
  keyword = '';
  loaiTranDauFilter = '';
  boCauHoiIdFilter: number | null = null;
  fromDateFilter = '';
  toDateFilter = '';

  showFilter = false;

  ngOnInit(): void {
    this.refreshData();
  }

  // ===================== THỐNG KÊ =====================
  loadStats(): void {
    this.loadingStats = true;
    this.tranDauService.getAdminStats().subscribe({
      next: (res: ResponseObject<any>) => {
        this.stats = res.data ?? this.stats;
        this.loadingStats = false;
      },
      error: () => {
        this.loadingStats = false;
      },
    });
  }

  // ===================== PHÒNG ĐANG CHỜ =====================
  loadPending(): void {
    this.loadingPending = true;
    this.tranDauService.getPendingBattles(0, 50).subscribe({
      next: (res: ResponseObject<PageResponse<TranDauResponse>>) => {
        this.pendingBattles = res.data?.items ?? [];
        this.loadingPending = false;
      },
      error: () => {
        this.loadingPending = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách phòng đang chờ', 'error');
      },
    });
  }

  viewRoomDetail(room: TranDauResponse): void {
    this.showRoomModal = true;
    this.roomModalLoading = true;
    this.roomDetail = null;

    this.tranDauService.adminGetRoomDetail(room.id).subscribe({
      next: (res: ResponseObject<any>) => {
        this.roomDetail = res.data;
        this.roomModalLoading = false;
      },
      error: () => {
        this.roomModalLoading = false;
        Swal.fire('Lỗi', 'Không thể tải chi tiết phòng', 'error');
        this.closeRoomModal();
      },
    });
  }

  closeRoomModal(): void {
    this.showRoomModal = false;
    this.roomDetail = null;
  }

  closeRoom(tranDauId: number): void {
    Swal.fire({
      title: 'Xác nhận đóng phòng?',
      text: 'Tất cả người chơi sẽ bị đuổi khỏi phòng.',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#d33',
      cancelButtonColor: '#3085d6',
      confirmButtonText: 'Đóng phòng',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.tranDauService.adminCloseRoom(tranDauId).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Đã đóng phòng', 'success');
            this.loadPending();
            this.loadStats();
            this.closeRoomModal();
          },
          error: (err) => {
            Swal.fire('Lỗi', err.error?.message || 'Không thể đóng phòng', 'error');
          },
        });
      }
    });
  }

  kickPlayer(tranDauId: number, userId: number, hoTen: string): void {
    Swal.fire({
      title: `Kick ${hoTen}?`,
      text: 'Người chơi sẽ bị đuổi khỏi phòng.',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#d33',
      cancelButtonColor: '#3085d6',
      confirmButtonText: 'Kick',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.tranDauService.adminKickPlayer(tranDauId, userId).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Đã kick người chơi', 'success');
            // Reload room detail
            if (this.roomDetail && this.roomDetail.id === tranDauId) {
              this.viewRoomDetail({id: tranDauId} as TranDauResponse);
            }
            this.loadPending();
          },
          error: (err) => {
            Swal.fire('Lỗi', err.error?.message || 'Không thể kick người chơi', 'error');
          },
        });
      }
    });
  }

  // ===================== LỊCH SỬ TRẬN ĐẤU =====================
  loadHistory(): void {
    this.loadingHistory = true;
    this.tranDauService
      .getAdminHistoryFiltered(
        this.historyPage,
        this.historyLimit,
        this.keyword || undefined,
        this.loaiTranDauFilter || undefined,
        this.boCauHoiIdFilter || undefined,
        this.fromDateFilter || undefined,
        this.toDateFilter || undefined
      )
      .subscribe({
        next: (res: ResponseObject<PageResponse<LichSuTranDauResponse>>) => {
          const pageData = res.data;
          this.historyItems = pageData?.items ?? [];
          this.historyTotalPages = pageData?.totalPages ?? 0;
          this.loadingHistory = false;
        },
        error: () => {
          this.loadingHistory = false;
          Swal.fire('Lỗi', 'Không thể tải lịch sử trận đấu', 'error');
        },
      });
  }

  onHistorySearch(): void {
    this.historyPage = 0;
    this.loadHistory();
  }

  clearFilters(): void {
    this.keyword = '';
    this.loaiTranDauFilter = '';
    this.boCauHoiIdFilter = null;
    this.fromDateFilter = '';
    this.toDateFilter = '';
    this.historyPage = 0;
    this.loadHistory();
  }

  goHistoryPage(page: number): void {
    if (page < 0 || page >= this.historyTotalPages) return;
    this.historyPage = page;
    this.loadHistory();
  }

  viewHistoryDetail(h: LichSuTranDauResponse): void {
    this.router.navigate(['/admin/tran-dau/history', h.lich_su_id]);
  }

  deleteHistory(lichSuId: number): void {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Lịch sử trận đấu sẽ bị xóa vĩnh viễn.',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#d33',
      cancelButtonColor: '#3085d6',
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
    }).then((result) => {
      if (result.isConfirmed) {
        this.tranDauService.adminDeleteHistory(lichSuId).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Đã xóa lịch sử', 'success');
            this.loadHistory();
            this.loadStats();
          },
          error: (err) => {
            Swal.fire('Lỗi', err.error?.message || 'Không thể xóa lịch sử', 'error');
          },
        });
      }
    });
  }

  // ===================== EXPORT CSV =====================
  exportCsv(): void {
    this.tranDauService
      .adminExportHistoryCsv(
        this.keyword || undefined,
        this.loaiTranDauFilter || undefined,
        this.boCauHoiIdFilter || undefined,
        this.fromDateFilter || undefined,
        this.toDateFilter || undefined
      )
      .subscribe({
        next: (blob: Blob) => {
          const url = window.URL.createObjectURL(blob);
          const a = document.createElement('a');
          a.href = url;
          a.download = `lich_su_tran_dau_${new Date().toISOString().split('T')[0]}.csv`;
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          window.URL.revokeObjectURL(url);
          Swal.fire('Thành công', 'Đã xuất file CSV', 'success');
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể xuất file CSV', 'error');
        },
      });
  }

  refreshData() {
    this.loadStats();
    this.loadPending();
    this.loadHistory();
  }
}
