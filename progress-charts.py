# Import necessary libraries
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# Load the data
section1 = pd.read_csv('csv-files/S23_Section_1_Grades.csv')
section2 = pd.read_csv('csv-files/S23_Section_2_Grades.csv')
#Generate these from Main.R first
s1p1 = pd.read_csv('csv-files/S1P1_Detailed.csv')
s1p2 = pd.read_csv('csv-files/S1P2_Detailed.csv')
s1p3 = pd.read_csv('csv-files/S1P3_Detailed.csv')
s2p1 = pd.read_csv('csv-files/S2P1_Detailed.csv')
s2p2 = pd.read_csv('csv-files/S2P2_Detailed.csv')
s2p3 = pd.read_csv('csv-files/S2P3_Detailed.csv')

s1points=pd.DataFrame()
s2points=pd.DataFrame()

def partial_charts(p1, p2, p3, p1name, p2name, p3name, piname):
  # Calculate differences between partials
  section1['d12'] = section1[p2] - section1[p1]
  section1['d23'] = section1[p3] - section1[p2]
  section1['d13'] = section1[p3] - section1[p1]
  
  section2['d12'] = section2[p2] - section2[p1]
  section2['d23'] = section2[p3] - section2[p2]
  section2['d13'] = section2[p3] - section2[p1]
  
  # Create a figure and a set of subplots
  fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(10, 15))
  
  # Plot for Section 1
  
  ## Sorting the dataframe by p1 and resetting the index
  section1_sorted = section1.sort_values(by=p1).reset_index()
  
  ## Finding Thirds
  total_students = len(section1_sorted)
  one_third_students_s1 = total_students // 3
  two_third_students_s1 = 2 * one_third_students_s1
  
  ## Plotting
  for i in section1_sorted.index:
      if i < one_third_students_s1:
          color = 'red'
      elif i < two_third_students_s1:
          color = 'blue'
      else:
          color = 'green'
      axes[0].plot([p1name, p2name, p3name], section1_sorted.loc[i, [p1, p2, p3]], marker='o', color=color)
  axes[0].set_title('Section 1 (JMCQ)')
  axes[0].set_ylabel('Score')
  axes[0].set_xlabel('Partial Test')
  
  # Plot for Section 2
  
  ## Sorting the dataframe by p1 and resetting the index
  section2_sorted = section2.sort_values(by=p1).reset_index()
  
  ## Finding Thirds
  total_students = len(section2_sorted)
  one_third_students_s2 = total_students // 3
  two_third_students_s2 = 2 * one_third_students_s2
  
  ## Plotting
  for i in section2_sorted.index:
      if i < one_third_students_s2:
          color = 'red'
      elif i < two_third_students_s2:
          color = 'blue'
      else:
          color = 'green'
      axes[1].plot([p1name, p2name, p3name], section2_sorted.loc[i, [p1, p2, p3]], marker='o', color=color)
  axes[1].set_title('Section 2 (MCQ)')
  axes[1].set_ylabel('Score')
  axes[1].set_xlabel('Partial Test')
  
  # Display the plot
  plt.tight_layout()
  #plt.show()
  # Save the plot to a PDF file
  fig.savefig('plots/student_progress_'+piname+'.pdf')
  
  
  
  # Create a figure and a set of subplots
  fig, axes = plt.subplots(nrows=2, ncols=3, figsize=(15, 10))
  
  # Plot for Section 1
  for i, diff in enumerate(['d12', 'd23', 'd13']):
      # Create separate data for each third of students
      data1 = section1_sorted.loc[section1_sorted.index < one_third_students_s1, diff]
      data2 = section1_sorted.loc[(section1_sorted.index >= one_third_students_s1) & (section1_sorted.index < two_third_students_s1), diff]
      data3 = section1_sorted.loc[section1_sorted.index >= two_third_students_s1, diff]
      # Plot the density plots with the corresponding colors
      sns.kdeplot(data1, ax=axes[0, i], color='red', label='First third')
      sns.kdeplot(data2, ax=axes[0, i], color='blue', label='Second third')
      sns.kdeplot(data3, ax=axes[0, i], color='green', label='Third third')
      axes[0,i].set_xlim(-100, 100)
      axes[0, i].set_title(f'Section 1 (JMCQ) - {diff}')
      axes[0, i].set_xlabel('Score Difference')
      axes[0, i].set_ylabel('Density')
      axes[0, i].legend()
  
  # Plot for Section 2
  for i, diff in enumerate(['d12', 'd23', 'd13']):
      # Create separate data for each third of students
      data1 = section2_sorted.loc[section2_sorted.index < one_third_students_s2, diff]
      data2 = section2_sorted.loc[(section2_sorted.index >= one_third_students_s2) & (section2_sorted.index < two_third_students_s2), diff]
      data3 = section2_sorted.loc[section2_sorted.index >= two_third_students_s2, diff]
      # Plot the density plots with the corresponding colors
      sns.kdeplot(data1, ax=axes[1, i], color='red', label='First third')
      sns.kdeplot(data2, ax=axes[1, i], color='blue', label='Second third')
      sns.kdeplot(data3, ax=axes[1, i], color='green', label='Third third')
      axes[1,i].set_xlim(-100, 100)
      axes[1, i].set_title(f'Section 2 (MCQ) - {diff}')
      axes[1, i].set_xlabel('Score Difference')
      axes[1, i].set_ylabel('Density')
      axes[1, i].legend()
  
  # Display the plot
  plt.tight_layout()
  #plt.show()
  # Save the plot to a PDF file
  fig.savefig(f'plots/progress_kde_{piname}.png')
  

def points_charts(points_type, s1points, s2points):
    # Specify the point columns to be used
    points_cols = [f'p{i}_{points_type}' for i in range(1, 4)]
  
    # Calculate differences between partials for each section
    for i in range(2):
        s1points[f'd{i+1}{i+2}'] = s1points[points_cols[i+1]] - s1points[points_cols[i]]
        s2points[f'd{i+1}{i+2}'] = s2points[points_cols[i+1]] - s2points[points_cols[i]]

    # Create a figure and a set of subplots
    fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(10, 15))

    # Define sections and titles for plotting
    sections = [s1points, s2points]
    titles = ['Section 1', 'Section 2']

    for section, title, ax in zip(sections, titles, axes):
        # Sort by first partial's points and reset index
        section_sorted = section.sort_values(by=points_cols[0]).reset_index()

        # Determine thirds of students
        total_students = len(section_sorted)
        one_third_students = total_students // 3
        two_third_students = 2 * one_third_students

        # Plot lines for each student
        for i in section_sorted.index:
            color = 'red' if i < one_third_students else 'blue' if i < two_third_students else 'green'
            ax.plot(['p1', 'p2', 'p3'], section_sorted.loc[i, points_cols], marker='o', color=color)

        # Set plot titles and labels
        ax.set_title(f'{title} ({points_type})')
        ax.set_ylabel('Points')
        ax.set_xlabel('Partial Test')

    # Show and save the progress plot
    plt.tight_layout()
    #plt.show()
    fig.savefig(f'plots/student_progress_{points_type}.pdf')

    # # Plot the differences
    # fig, axes = plt.subplots(nrows=2, ncols=3, figsize=(15, 10))

    # # Loop over differences
    # for diff_index, ax_row in enumerate(axes):
    #     for i, ax in enumerate(ax_row):
    #         # Create separate data for each third of students
    #         data1 = sections[diff_index].loc[sections[diff_index].index < one_third_students, f'd{i+1}{i+2}']
    #         data2 = sections[diff_index].loc[(sections[diff_index].index >= one_third_students) & (sections[diff_index].index < two_third_students), f'd{i+1}{i+2}']
    #         data3 = sections[diff_index].loc[sections[diff_index].index >= two_third_students, f'd{i+1}{i+2}']

    #         # Plot the density plots with the corresponding colors
    #         sns.kdeplot(data1, ax=ax, color='red', label='First third')
    #         sns.kdeplot(data2, ax=ax, color='blue', label='Second third')
    #         sns.kdeplot(data3, ax=ax, color='green', label='Third third')
    #         ax.set_title(f'{titles[diff_index]} ({points_type}) - d{i+1}{i+2}')
    #         ax.set_xlabel('Score Difference')
    #         ax.set_ylabel('Density')
    #         ax.legend()

    # # Show and save the differences plot
    # plt.tight_layout()
    # #plt.show()
    # fig.savefig(f'plots/progress_kde_{points_type}.png')
  
  
def points_combine():
  # Select only 'mcq_points' and 'justification_points' from each dataframe and rename columns
  s1p1_selected = s1p1[['mcq_points', 'justification_points','j_gain']].copy()
  s1p1_selected.columns = ['p1_mcq_points', 'p1_justification_points', 'p1_j_gain']
  
  s1p2_selected = s1p2[['mcq_points', 'justification_points','j_gain']].copy()
  s1p2_selected.columns = ['p2_mcq_points', 'p2_justification_points', 'p2_j_gain']
  
  s1p3_selected = s1p3[['mcq_points', 'justification_points','j_gain']].copy()
  s1p3_selected.columns = ['p3_mcq_points', 'p3_justification_points', 'p3_j_gain']
  
  # Combine these selected parts into a new dataframe
  s1points = pd.concat([s1p1_selected, s1p2_selected, s1p3_selected], axis=1)
  
  
  # Select only 'mcq_points' and 'justification_points' from each dataframe and rename columns
  s2p1_selected = s2p1[['mcq_points', 'justification_points', 'j_gain']].copy()
  s2p1_selected.columns = ['p1_mcq_points', 'p1_justification_points', 'p1_j_gain']
  
  s2p2_selected = s2p2[['mcq_points', 'justification_points', 'j_gain']].copy()
  s2p2_selected.columns = ['p2_mcq_points', 'p2_justification_points', 'p2_j_gain']
  
  s2p3_selected = s2p3[['mcq_points', 'justification_points', 'j_gain']].copy()
  s2p3_selected.columns = ['p3_mcq_points', 'p3_justification_points', 'p3_j_gain']
  
  # Combine these selected parts into a new dataframe
  s2points = pd.concat([s2p1_selected, s2p2_selected, s2p3_selected], axis=1)
  
  return s1points, s2points


# Plot partials for each section
partial_charts('P1.After.Comments','P2.After.Comments','P3.After.Comments',
'P1','P2','P3','Pi')
partial_charts('P1.Real','P2.Real','P3.Real', 
'P1raw','P2raw','P3raw','Piraw')
partial_charts('P1.New','P2.New','P3.New',
'P1+','P2+','P3+','Pi+')

s1points, s2points = points_combine()

points_charts('mcq_points', s1points, s2points)
points_charts('justification_points', s1points, s2points)
points_charts('j_gain', s1points, s2points)




  
