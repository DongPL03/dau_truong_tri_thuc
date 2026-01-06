import { CommonModule } from '@angular/common';
import { Component, OnInit, inject, signal } from '@angular/core';
import Swal from 'sweetalert2';
import { NhanThuongNhiemVuResponse, NhiemVuResponse, QuestItem } from '../../models/nhiem-vu.model';
import { NhiemVuService } from '../../services/nhiem-vu.service';

@Component({
  selector: 'app-nhiem-vu',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './nhiem-vu.html',
  styleUrl: './nhiem-vu.scss',
})
export class NhiemVuComponent implements OnInit {
  private nhiemVuService = inject(NhiemVuService);

  loading = signal(true);
  claiming = signal<string | null>(null);
  activeTab = signal<'daily' | 'weekly'>('daily');

  dailyQuests = signal<QuestItem[]>([]);
  weeklyQuests = signal<QuestItem[]>([]);
  dailyCompleted = signal(0);
  dailyTotal = signal(0);
  weeklyCompleted = signal(0);
  weeklyTotal = signal(0);

  ngOnInit(): void {
    this.loadQuests();
  }

  loadQuests(): void {
    this.loading.set(true);
    this.nhiemVuService.getQuests().subscribe({
      next: (res: NhiemVuResponse) => {
        this.dailyQuests.set(res.daily_quests);
        this.weeklyQuests.set(res.weekly_quests);
        this.dailyCompleted.set(res.daily_completed);
        this.dailyTotal.set(res.daily_total);
        this.weeklyCompleted.set(res.weekly_completed);
        this.weeklyTotal.set(res.weekly_total);
        this.loading.set(false);
      },
      error: (err) => {
        console.error('Error loading quests:', err);
        this.loading.set(false);
        Swal.fire('Lỗi', 'Không thể tải danh sách nhiệm vụ', 'error');
      },
    });
  }

  setTab(tab: 'daily' | 'weekly'): void {
    this.activeTab.set(tab);
  }

  get currentQuests(): QuestItem[] {
    return this.activeTab() === 'daily' ? this.dailyQuests() : this.weeklyQuests();
  }

  get hasClaimable(): boolean {
    const quests = this.activeTab() === 'daily' ? this.dailyQuests() : this.weeklyQuests();
    return quests.some((q) => q.da_hoan_thanh && !q.da_nhan_thuong);
  }

  claimReward(quest: QuestItem): void {
    if (!quest.da_hoan_thanh || quest.da_nhan_thuong) return;

    this.claiming.set(quest.ma);
    this.nhiemVuService.claimReward(quest.ma).subscribe({
      next: (res: NhanThuongNhiemVuResponse) => {
        this.claiming.set(null);
        if (res.thanh_cong) {
          this.showRewardPopup(res);
          this.loadQuests(); // Refresh
        } else {
          Swal.fire('Thông báo', res.thong_bao, 'info');
        }
      },
      error: (err) => {
        this.claiming.set(null);
        console.error('Error claiming reward:', err);
        Swal.fire('Lỗi', 'Không thể nhận thưởng', 'error');
      },
    });
  }

  claimAll(): void {
    this.claiming.set('all');
    this.nhiemVuService.claimAllRewards().subscribe({
      next: (res: NhanThuongNhiemVuResponse) => {
        this.claiming.set(null);
        if (res.thanh_cong) {
          this.showRewardPopup(res);
          this.loadQuests();
        } else {
          Swal.fire('Thông báo', res.thong_bao, 'info');
        }
      },
      error: (err) => {
        this.claiming.set(null);
        console.error('Error claiming all rewards:', err);
        Swal.fire('Lỗi', 'Không thể nhận thưởng', 'error');
      },
    });
  }

  private showRewardPopup(res: NhanThuongNhiemVuResponse): void {
    const rewardsHtml =
      res.phan_thuong
        ?.map(
          (r) => `
      <div class="reward-item">
        <span class="reward-icon">${r.icon}</span>
        <span class="reward-name">${r.ten}</span>
        <span class="reward-qty">+${r.so_luong}</span>
      </div>
    `
        )
        .join('') || '';

    Swal.fire({
      title: '🎉 Nhận thưởng thành công!',
      html: `
        <div class="rewards-popup">
          ${rewardsHtml}
        </div>
        <div class="gold-total">
          💰 Số dư: <strong>${res.gold_moi?.toLocaleString()}</strong> xu
        </div>
      `,
      icon: 'success',
      confirmButtonText: 'Tuyệt vời!',
      customClass: {
        popup: 'quest-reward-popup',
      },
    });
  }

  getProgressWidth(quest: QuestItem): string {
    return `${Math.min(100, quest.phan_tram)}%`;
  }

  getQuestStatusClass(quest: QuestItem): string {
    if (quest.da_nhan_thuong) return 'claimed';
    if (quest.da_hoan_thanh) return 'completed';
    return 'in-progress';
  }
}
