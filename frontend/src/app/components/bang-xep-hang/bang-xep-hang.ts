import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';

import {Base} from '../base/base';
import {LeaderboardEntryResponse} from '../../responses/bangxephang/leaderboard-entry-response';
import {ResponseObject} from '../../responses/response-object';
import {PageResponse} from '../../responses/page-response';
import {FormsModule} from '@angular/forms';
import {ChuDe} from '../../models/chude';
import {Bocauhoi} from '../../models/bocauhoi';
import Swal from 'sweetalert2';
import {UserSummaryResponse} from '../../responses/nguoidung/user-summary-response';
import {LichSuTranDauResponse} from '../../responses/trandau/lichsutrandau';
import {RouterLink} from '@angular/router';

@Component({
  selector: 'app-bang-xep-hang',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink],
  templateUrl: './bang-xep-hang.html',
  styleUrl: './bang-xep-hang.scss'
})
export class BangXepHang extends Base implements OnInit {

  loading = false;

  page = 0;
  limit = 20;
  totalPages = 0;

  items: LeaderboardEntryResponse[] = [];
  chu_de_options: ChuDe[] = [];
  bo_cau_hoi_options: Bocauhoi[] = [];

  // 🔍 filter/sort
  search_term: string = '';
  sort_mode: 'POINTS' | 'WINRATE' | 'MATCHES' = 'POINTS';
  min_matches: number = 0; // 0 = không lọc

  time_range: 'ALL' | 'WEEK' | 'MONTH' = 'ALL';
  selected_chu_de_id?: number;
  selected_bo_cau_hoi_id?: number;
  friend_only: boolean = false; // có thể tạm disable

  // --- modal user detail ---
  show_user_modal = false;
  selected_user_id?: number;

  user_loading = false;
  user_summary?: UserSummaryResponse | null;

  user_history_loading = false;
  user_history_items: LichSuTranDauResponse[] = [];
  user_history_page = 0;
  user_history_limit = 5;
  user_history_total_pages = 0;

  me = this.userService.getUserResponseFromLocalStorage();


  ngOnInit() {
    this.loadPage(0);
    this.loadChuDe();
  }

  /** Gọi API lấy danh sách chủ đề */
  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chu_de_options = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then(r => {
        });
      }
    });
  }

  loadPage(page: number) {
    if (page < 0) {
      return;
    }
    this.loading = true;

    this.leaderboardService.getGlobal(page, this.limit, this.time_range, this.selected_chu_de_id, this.selected_bo_cau_hoi_id,).subscribe({
      next: (res: ResponseObject<PageResponse<LeaderboardEntryResponse>>) => {
        this.loading = false;
        const data = res.data;
        if (data) {
          this.items = data.items ?? [];
          this.page = data.currentPage ?? 0;
          this.totalPages = data.totalPages ?? 0;
        } else {
          this.items = [];
          this.page = 0;
          this.totalPages = 0;
        }
      },
      error: err => {
        this.loading = false;
        this.handleApiError(err);
      }
    });
  }

  filteredItems(): LeaderboardEntryResponse[] {
    let list = this.items ?? [];

    // 1️⃣ Filter theo keyword (tên hoặc ID)
    const keyword = this.search_term.trim().toLowerCase();
    if (keyword) {
      list = list.filter(item => {
        const name = item.ho_ten ? item.ho_ten.toLowerCase() : '';
        const idStr = (item.user_id ?? '').toString();
        return name.includes(keyword) || idStr.includes(keyword);
      });
    }

    // 2️⃣ Filter theo số trận tối thiểu
    if (this.min_matches > 0) {
      list = list.filter(item => (item.tong_tran || 0) >= this.min_matches);
    }

    // 3️⃣ Sort theo mode
    list = [...list]; // clone để không đụng mảng gốc

    switch (this.sort_mode) {
      case 'WINRATE':
        list.sort((a, b) => (b.ti_le_thang || 0) - (a.ti_le_thang || 0));
        break;
      case 'MATCHES':
        list.sort((a, b) => (b.tong_tran || 0) - (a.tong_tran || 0));
        break;
      case 'POINTS':
      default:
        list.sort((a, b) => (b.tong_diem || 0) - (a.tong_diem || 0));
        break;
    }

    return list;
  }

  goToPage(p: number) {
    if (p < 0 || p >= this.totalPages) {
      return;
    }
    this.loadPage(p);
  }

  isTop(rank: number): boolean {
    return rank === 1 || rank === 2 || rank === 3;
  }

  private handleApiError(err: any) {
    console.error('API Error:', err);
  }

  openUserModal(item: LeaderboardEntryResponse) {
    this.selected_user_id = item.user_id;
    this.show_user_modal = true;

    this.loadUserSummary();
    this.loadUserHistory(0);
  }

  closeUserModal() {
    this.show_user_modal = false;
    this.user_summary = null;
    this.user_history_items = [];
  }

  private loadUserSummary() {
    if (!this.selected_user_id) return;
    this.user_loading = true;

    this.userService.getUserSummary(this.selected_user_id).subscribe({
      next: (res) => {
        this.user_loading = false;
        this.user_summary = res.data ?? null;
      },
      error: (err) => {
        this.user_loading = false;
        this.handleApiError(err);
      }
    });
  }

  loadUserHistory(page: number = 0) {
    if (!this.selected_user_id) return;
    this.user_history_loading = true;

    this.tranDauService.getUserHistory(this.selected_user_id, page, this.user_history_limit)
      .subscribe({
        next: (res) => {
          this.user_history_loading = false;
          const p = res.data!;
          this.user_history_items = p.items ?? [];
          this.user_history_page = p.currentPage ?? 0;
          this.user_history_total_pages = p.totalPages ?? 0;
        },
        error: (err) => {
          this.user_history_loading = false;
          this.handleApiError(err);
        }
      });
  }

  changeUserHistoryPage(p: number) {
    if (p < 0 || p >= this.user_history_total_pages) return;
    this.loadUserHistory(p);
  }


  mapTierLabel(tier: string | null | undefined): string {
    switch ((tier || '').toUpperCase()) {
      case 'MASTER':
        return 'Cao thủ';
      case 'DIAMOND':
        return 'Kim cương';
      case 'PLATINUM':
        return 'Bạch kim';
      case 'GOLD':
        return 'Vàng';
      case 'SILVER':
        return 'Bạc';
      case 'BRONZE':
      default:
        return 'Đồng';
    }
  }

}
