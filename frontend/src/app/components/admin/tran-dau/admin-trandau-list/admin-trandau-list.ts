import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';

import {Base} from '../../../base/base';
import {TranDauResponse} from '../../../../responses/trandau/trandau-response';
import {LichSuTranDauResponse} from '../../../../responses/trandau/lichsutrandau';
import {ResponseObject} from '../../../../responses/response-object';
import {PageResponse} from '../../../../responses/page-response';

@Component({
  selector: 'app-admin-trandau-list',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-trandau-list.html',
  styleUrl: './admin-trandau-list.scss'
})
export class AdminTranDauList extends Base implements OnInit {

  // phòng đang chờ
  loadingPending = false;
  pendingBattles: TranDauResponse[] = [];

  // lịch sử tất cả trận
  loadingHistory = false;
  historyItems: LichSuTranDauResponse[] = [];
  historyPage = 0;
  historyLimit = 10;
  historyTotalPages = 0;

  // filter client-side cho lịch sử
  keyword = '';
  filteredHistory: LichSuTranDauResponse[] = [];


  ngOnInit(): void {
    this.loadPending();
    this.loadHistory();
  }

  loadPending(): void {
    this.loadingPending = true;
    this.tranDauService.getPendingBattles(0, 20).subscribe({
      next: (res: ResponseObject<PageResponse<TranDauResponse>>) => {
        this.pendingBattles = res.data?.items ?? [];
        this.loadingPending = false;
      },
      error: () => {
        this.loadingPending = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách phòng đang chờ', 'error').then(r => {
        });
      }
    });
  }

  loadHistory(): void {
    this.loadingHistory = true;
    this.tranDauService.getAllHistory(this.historyPage, this.historyLimit)
      .subscribe({
        next: (res: ResponseObject<PageResponse<LichSuTranDauResponse>>) => {
          const pageData = res.data;
          this.historyItems = pageData?.items ?? [];
          this.historyTotalPages = pageData?.totalPages ?? 0;
          this.applyHistoryFilter();
          this.loadingHistory = false;
        },
        error: () => {
          this.loadingHistory = false;
          Swal.fire('Lỗi', 'Không thể tải lịch sử trận đấu', 'error').then(r => {
          });
        }
      });
  }

  applyHistoryFilter(): void {
    if (!this.keyword?.trim()) {
      this.filteredHistory = [...this.historyItems];
      return;
    }

    const kw = this.keyword.trim().toLowerCase();

    this.filteredHistory = this.historyItems.filter((h) => {
      const tenPhong = h.ten_phong?.toLowerCase() ?? '';
      const boTieuDe = h.bo_cau_hoi_tieu_de?.toLowerCase() ?? '';
      const luat = h.luat_tinh_diem?.toLowerCase() ?? '';

      return tenPhong.includes(kw)
        || boTieuDe.includes(kw)
        || luat.includes(kw)
        || String(h.tran_dau_id).includes(kw)
        || String(h.lich_su_id).includes(kw);
    });
  }

  viewHistoryDetail(h: LichSuTranDauResponse): void {
    this.router.navigate(['/admin/tran-dau/history', h.lich_su_id]).then(r => {
    });
  }


  onHistorySearch(): void {
    this.historyPage = 0;
    this.loadHistory();
  }

  goHistoryPage(page: number): void {
    if (page < 0 || page >= this.historyTotalPages) return;
    this.historyPage = page;
    this.loadHistory();
  }

}
