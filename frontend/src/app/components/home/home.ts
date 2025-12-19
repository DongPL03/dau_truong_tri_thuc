import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ResponseObject} from '../../responses/response-object';
import {TranDauResponse} from '../../responses/trandau/trandau-response';
import {BoCauHoiResponse} from '../../responses/bocauhoi/bocauhoi-response';
import {PageResponse} from '../../responses/page-response';
import {UserResponse} from '../../responses/nguoidung/user-response';
import {Base} from '../base/base';
import {UserSummaryResponse} from '../../responses/nguoidung/user-summary-response';
import {LichSuTranDauResponse as LichSuTranDauShort} from '../../responses/lichsutrandau/lich_su_tran_dau_response';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './home.html',
  styleUrl: './home.scss'
})
export class Home extends Base implements OnInit {
  pendingBattles: TranDauResponse[] = [];
  featuredQuizzes: BoCauHoiResponse[] = [];
  user?: UserResponse | null;

  user_summary?: UserSummaryResponse | null;
  recent_matches: LichSuTranDauShort[] = [];
  loading_summary = false;


  ngOnInit() {
    this.user = this.userService.getUserResponseFromLocalStorage();

    if (this.user?.id) {
      this.loadUserSummary(this.user.id);
    }

    this.loadPendingBattles();
    this.loadFeaturedQuizzes();
  }


  loadPendingBattles() {
    this.tranDauService.getPendingBattles(0, 5).subscribe({
      next: (res: ResponseObject<PageResponse<TranDauResponse>>) => {
        this.pendingBattles = res.data?.items ?? [];
      },
      error: (err) => console.error('❌ Lỗi khi tải danh sách bộ câu hỏi:', err)
    });
  }

  loadFeaturedQuizzes() {
    this.bocauHoiService.getFeatured(3).subscribe({
      next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
        this.featuredQuizzes = res.data?.items ?? [];
      },
      error: (err) => console.error('❌ Lỗi khi tải danh sách bộ câu hỏi:', err)
    });
  }

  // Lấy thống kê tổng quan + lịch sử gần đây của user
  loadUserSummary(user_id: number) {
    this.loading_summary = true;

    this.userService.getUserSummary(user_id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        console.log('✅ Thống kê người dùng tải về:', res.data);
        this.loading_summary = false;
        this.user_summary = res.data ?? null;
        if (this.user_summary) {
          // Tính toán tỉ lệ thắng
          const {so_tran_thang, tong_tran} = this.user_summary;
          this.user_summary.ti_le_thang = tong_tran > 0 ? so_tran_thang / tong_tran : 0;
        }
        console.log('User Summary:', this.user_summary);

        // Lấy tối đa 3 trận gần nhất từ lich_su_tran_dau (nếu backend có trả)
        const history = this.user_summary?.lich_su_tran_dau ?? [];
        this.recent_matches = history.slice(0, 3);
      },
      error: (err) => {
        this.loading_summary = false;
        console.error('❌ Lỗi khi tải thống kê người dùng:', err);
      }
    });
  }

  claimWeeklyReward() {
    this.leaderboardService.claimWeeklyRankReward().subscribe({
      next: res => {
        const data = res.data!;
        if (data.claimed_before) {
          this.toastService.show(
            `Tuần ${data.week_id} bạn đã nhận thưởng rồi`,
            'info'
          );
          return;
        }

        // cập nhật vàng hiện tại trên UI (nếu bạn có state tien_vang)
        if (this.user_summary) {
          this.user_summary.tien_vang = data.gold_after;
        }

        Swal.fire({
          icon: 'success',
          title: '🎁 Thưởng xếp hạng tuần',
          html: `
          <p>Rank hiện tại: <strong>${data.rank_tier}</strong>
             (hạng #${data.global_rank || 'N/A'})</p>
          <p>Nhận được: <strong>+${data.gold_reward} vàng</strong></p>
          <p>Vàng sau khi nhận: <strong>${data.gold_after}</strong></p>
        `,
          confirmButtonText: 'OK'
        }).then(r => {
        });
      },
      error: err => {
        const msg = err?.error?.message || 'Không thể nhận thưởng xếp hạng tuần';
        this.toastService.show(msg, 'error');
      }
    });
  }


  navigateQuiz() {
    this.router.navigate(['/bo-cau-hoi/danh-sach-bo-cau-hoi']).then(r => {
    });
  }

  navigateBattle() {
    this.router.navigate(['/tran-dau/pending']).then(r => {
    });
  }

  navigateHistory() {
    this.router.navigate(['/tran-dau/lich-su-tran-dau']).then(r => {
    });
  }

  navigatePractice() {
    this.router.navigate(['/luyen-tap']).then(r => {
    });
  }

  navigateFriend(): void {
    this.router.navigate(['/ban-be']).then(r => {
    });
  }


}
