// src/app/components/admin/dashboard/admin-dashboard.ts
import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';

import {Base} from '../../base/base';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {ChartConfiguration, ChartOptions} from 'chart.js';
import {BaseChartDirective} from 'ng2-charts';
import {AdminDashboardStatsResponse} from '../../../responses/admin/admin-dashboard-stats-response';
import {AdminSummaryStatsResponse} from '../../../responses/thongke/admin-summary-stats-response';
import {DateCountResponse} from '../../../responses/thongke/date-count-response';
import {TopBoCauHoiStatsResponse} from '../../../responses/thongke/top-bo-cau-hoi-stats-response';
import Swal from 'sweetalert2';


@Component({
  selector: 'app-admin-dashboard',
  standalone: true,
  imports: [CommonModule, BaseChartDirective],
  templateUrl: './admin-dashboard.html',
  styleUrls: ['./admin-dashboard.scss']
})
export class AdminDashboard extends Base implements OnInit {

  loading = false;

  // KPI
  total_users = 0;
  total_bo_cau_hoi = 0;
  pending_bo_cau_hoi = 0;
  total_tran_dau = 0;

  // danh sách bộ câu hỏi chờ duyệt (gần đây)
  pending_list: BoCauHoiResponse[] = [];

  loading_summary = false;
  loading_battles_chart = false;
  loading_top_bo = false;

  // --- data ---
  summary: AdminSummaryStatsResponse | null = null;
  battles_by_day: DateCountResponse[] = [];
  top_bo_cau_hoi: TopBoCauHoiStatsResponse[] = [];

  // nếu bạn dùng chart lib (Apex / Chart.js), có thể map ra dạng label/value:
  battle_chart_labels: string[] = [];
  battle_chart_values: number[] = [];


  ngOnInit(): void {
    this.loadKpi();
    this.loadPendingBoCauHoi();
    this.loadUserNewPerDay(7);
    this.loadBoCauHoiStatusStats();
    this.load_summary();
    this.load_battles_by_day();
    this.load_top_bo_cau_hoi();
  }

  /** Load các chỉ số KPI */
  loadKpi(): void {
    this.loading = true;

    this.adminService.get_dashboard_stats().subscribe({
      next: (res: ResponseObject<AdminDashboardStatsResponse>) => {
        const data = res.data!;
        console.log('Dashboard stats:', data);
        this.total_users = data.tong_nguoi_dung;
        this.total_bo_cau_hoi = data.tong_bo_cau_hoi;
        this.pending_bo_cau_hoi = data.tong_bo_cau_hoi_cho_duyet;
        this.total_tran_dau = data.tong_tran_dau;

        this.loading = false;
        this.updateKpiChart();
      },
      error: () => {
        this.loading = false;
      }
    });
  }


  /** Load danh sách bộ câu hỏi chờ duyệt gần đây */
  loadPendingBoCauHoi(): void {
    this.bocauHoiService.getAll(
      '',
      0,
      '',
      'CHO_DUYET',
      'NEWEST',
      0,
      5              // lấy top 5 bộ chờ duyệt
    ).subscribe({
      next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
        const data = res.data;
        this.pending_list = data?.items || [];
      },
      error: () => {
        this.pending_list = [];
      }
    });
  }

  loadUserNewPerDay(days: number = 7): void {
    this.adminService.get_user_new_per_day(days).subscribe({
      next: (res) => {
        const data = res.data || [];

        const labels = data.map(item => {
          // item.ngay = "2025-11-25" → hiển thị "25/11"
          const [year, month, day] = item.ngay.split('-');
          return `${day}/${month}`;
        });

        const values = data.map(item => item.so_luong);

        this.userNewLineChartData = {
          labels,
          datasets: [
            {
              ...this.userNewLineChartData.datasets[0],
              data: values
            }
          ]
        };
      },
      error: () => {
        // có thể set data rỗng
        this.userNewLineChartData = {
          labels: [],
          datasets: [
            {
              ...this.userNewLineChartData.datasets[0],
              data: []
            }
          ]
        };
      }
    });
  }

  /** Load thống kê bộ câu hỏi theo trạng thái */
  loadBoCauHoiStatusStats(): void {
    this.adminService.get_bo_cau_hoi_status_stats().subscribe({
      next: (res) => {
        const data = res.data;
        if (!data) {
          return;
        }

        const daDuyet = data.so_luong_da_duyet || 0;
        const choDuyet = data.so_luong_cho_duyet || 0;
        const tuChoi = data.so_luong_tu_choi || 0;

        this.boCauHoiDonutData = {
          ...this.boCauHoiDonutData,
          datasets: [
            {
              ...this.boCauHoiDonutData.datasets[0],
              data: [daDuyet, choDuyet, tuChoi]
            }
          ]
        };
      },
      error: () => {
        // có thể để nguyên data cũ hoặc reset về 0
      }
    });
  }

  load_summary(): void {
    this.loading_summary = true;
    this.adminStatsService.get_summary().subscribe({
      next: (res: ResponseObject<AdminSummaryStatsResponse>) => {
        this.summary = res.data ?? null;
        this.loading_summary = false;
      },
      error: (err: any) => {
        console.error('get_summary error', err);
        this.loading_summary = false;
        Swal.fire('Lỗi', 'Không lấy được thống kê tổng quan hệ thống', 'error').then(r => {
        });
      }
    });
  }

  load_battles_by_day(days: number = 7): void {
    this.loading_battles_chart = true;
    this.adminStatsService.get_battles_by_day(days).subscribe({
      next: (res: ResponseObject<DateCountResponse[]>) => {
        this.battles_by_day = res.data ?? [];
        this.loading_battles_chart = false;

        // map sang mảng labels / values cho chart
        this.battle_chart_labels = this.battles_by_day.map(d => d.ngay);
        this.battle_chart_values = this.battles_by_day.map(d => d.so_luong);
      },
      error: (err: any) => {
        console.error('get_battles_by_day error', err);
        this.loading_battles_chart = false;
        Swal.fire('Lỗi', 'Không lấy được thống kê số trận theo ngày', 'error').then(r => {
        });
      }
    });
  }

  load_top_bo_cau_hoi(limit: number = 5): void {
    this.loading_top_bo = true;
    this.adminStatsService.get_top_bo_cau_hoi(limit).subscribe({
      next: (res: ResponseObject<TopBoCauHoiStatsResponse[]>) => {
        this.top_bo_cau_hoi = res.data ?? [];
        this.loading_top_bo = false;
      },
      error: (err) => {
        console.error('get_top_bo_cau_hoi error', err);
        this.loading_top_bo = false;
        Swal.fire('Lỗi', 'Không lấy được top bộ câu hỏi', 'error').then(r => {
        });
      }
    });
  }


  // Chart: user mới theo ngày
  userNewLineChartData: ChartConfiguration<'line'>['data'] = {
    labels: [],
    datasets: [
      {
        data: [],
        label: 'User mới / ngày',
        fill: true,
        tension: 0.3,
        pointRadius: 3,
        borderWidth: 2
      }
    ]
  };

  userNewLineChartOptions: ChartOptions<'line'> = {
    responsive: true,
    maintainAspectRatio: false,
    scales: {
      x: {
        ticks: {color: '#e5e7eb'},
        grid: {color: 'rgba(148,163,184,0.15)'}
      },
      y: {
        beginAtZero: true,
        ticks: {color: '#e5e7eb'},
        grid: {color: 'rgba(148,163,184,0.15)'}
      }
    },
    plugins: {
      legend: {
        labels: {color: '#000'}
      }
    }
  };


  // Chart: tổng quan User vs Trận đấu
  kpiBarChartData: ChartConfiguration<'bar'>['data'] = {
    labels: ['Người dùng', 'Trận đấu'],
    datasets: [
      {
        data: [this.total_users, this.total_tran_dau],
        label: 'Số lượng',
        backgroundColor: ['rgba(56, 189, 248, 0.6)', 'rgba(129, 140, 248, 0.6)'],
        borderColor: ['rgba(56, 189, 248, 1)', 'rgba(129, 140, 248, 1)'],
        borderWidth: 1
      }
    ]
  };

  kpiBarChartOptions: ChartOptions<'bar'> = {
    responsive: true,
    maintainAspectRatio: false,
    scales: {
      x: {
        ticks: {color: '#e5e7eb'},
        grid: {color: 'rgba(148,163,184,0.2)'}
      },
      y: {
        beginAtZero: true,
        ticks: {color: '#e5e7eb'},
        grid: {color: 'rgba(148,163,184,0.2)'}
      }
    },
    plugins: {
      legend: {
        labels: {color: '#000'}
      }
    }
  };

  updateKpiChart(): void {
    this.kpiBarChartData = {
      ...this.kpiBarChartData,
      datasets: [
        {
          ...this.kpiBarChartData.datasets[0],
          data: [this.total_users, this.total_tran_dau]
        }
      ]
    };
  }

  // Donut chart: bộ câu hỏi theo trạng thái
  boCauHoiDonutData: ChartConfiguration<'doughnut'>['data'] = {
    labels: ['Đã duyệt', 'Chờ duyệt', 'Từ chối'],
    datasets: [
      {
        data: [0, 0, 0],
        // màu có thể để chart.js auto, hoặc set thủ công:
        backgroundColor: [
          'rgba(34, 197, 94, 0.8)',   // xanh lá
          'rgba(245, 158, 11, 0.8)',  // vàng
          'rgba(239, 68, 68, 0.8)'    // đỏ
        ],
        borderColor: [
          'rgba(34, 197, 94, 1)',
          'rgba(245, 158, 11, 1)',
          'rgba(239, 68, 68, 1)'
        ],
        borderWidth: 1
      }
    ]
  };

  boCauHoiDonutOptions: ChartOptions<'doughnut'> = {
    responsive: true,
    maintainAspectRatio: false,
    plugins: {
      legend: {
        position: 'bottom',
        labels: {
          color: '#000'
        }
      }
    }
  };


  // 🔗 Quick links
  goToBoCauHoi(): void {
    this.router.navigate(['/admin/bo-cau-hoi']).then(r => {
    });
  }

  goToUsers(): void {
    this.router.navigate(['/admin/users']).then(r => {
    });
  }

  goToTranDau(): void {
    this.router.navigate(['/admin/tran-dau']).then(r => {
    });
  }

  goToThongKe(): void {
    this.router.navigate(['/admin/thong-ke']).then(r => {
    });
  }
}
